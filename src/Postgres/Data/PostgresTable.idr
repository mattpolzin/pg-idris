module Postgres.Data.PostgresTable

import Postgres.Data.PostgresType
import public Data.DPair

import Postgres.Data.PostgresValue
import Data.List
import public Data.List.Elem
import Data.Vect
import Data.Vect.Elem
import public Data.Vect.Quantifiers
import Data.String.Extra

%default total

||| Either an identifier (table name) or subquery (select statement).
public export
data TableStatement = Identifier String
                    | ||| A subquery identified AS the first arguemnt and SELECTED by the second argument.
                      Subquery String String

export
Show TableStatement where
  show (Identifier n) = "\"\{n}\""
  show (Subquery n str) = "(\{str}) AS \"\{n}\""

||| Some representation of a Postgres table.
public export
interface PostgresTable t where
  ||| A table name or subselect statement from which the following columns could be selected.
  tableStatement : t -> TableStatement
  ||| The columns this table offers. Column names should not include double quotes, even where they
  ||| are needed when written down in SQL statements.
  columns : t -> List (String, Exists PColType)

||| A runtime representation of a table. These have string fields and table statements so as to not limit
||| themselves to nominal postgres tables; you can also represent joins naturally where the table statement is
||| a SELECT with a JOIN instead of just a table's name.
public export
record RuntimeTable where
  constructor RT
  tableStatement : TableStatement
  columns : List (String, Exists PColType)

export
implementation PostgresTable RuntimeTable where
  tableStatement = .tableStatement
  columns = .columns

public export
col : (nullable : Nullability) -> (pt : PType) -> Exists PColType
col nullable pt = Evidence pt (MkColType nullable pt)

||| A mapping between a column name and Idris type to some element in a list of column names
||| and Postgres types. This mapping proves that the column names exists and that the Postgres
||| type for that column can be cast to the Idris type specified.
public export
data ColumnMapping : List (String, Exists PColType) -> (String, Type) -> Type where
  HereNul : (name : String) -> (ty : Type) -> ValueCast pt ty => ColumnMapping ((name, (Evidence pt (MkColType Nullable pt))) :: xs) (name, Maybe ty)
  Here : (name : String) -> (ty : Type) -> ValueCast pt ty => ColumnMapping ((name, (Evidence pt (MkColType NonNullable pt))) :: xs) (name, ty)
  There : ColumnMapping xs (name, ty) -> ColumnMapping (x :: xs) (name, ty)

public export
HasMappings : PostgresTable t => (table : t) -> (cols : Vect n (String, Type)) -> Type
HasMappings table cols = All (ColumnMapping (columns table)) cols

||| Create a select statement based on the columns you would like to grab from the
||| given table.
public export
select : PostgresTable t => (table : t) -> (cols : Vect n (String, Type)) -> (0 _ : HasMappings table cols) => String
select table cols =
  let tableStatement = show $ tableStatement table
      columnNames    = join "," $ quoteColumn . fst <$> (toList cols)
  in  "SELECT \{columnNames} FROM \{tableStatement}"
  where
    quoteColumn : String -> String
    quoteColumn str = "\"\{str}\""

public export
data Join : t1 -> t2 -> Type where
  On : PostgresTable t => PostgresTable u => {t1 : t} -> {t2 : u} -> (c1 : String) -> (c2 : String) -> (0 _ : Elem c1 (Builtin.fst <$> (columns t1))) => (0 _ : Elem c2 (Builtin.fst <$> (columns t2))) => Join t1 t2

namespace Join
  public export
  column1 : Join t u -> String
  column1 (On c1 c2) = c1

  public export
  column2 : Join t u -> String
  column2 (On c1 c2) = c2

public export
innerJoin : PostgresTable t => PostgresTable u => (table1 : t) -> (table2 : u) -> (on : Join table1 table2) -> RuntimeTable
innerJoin table1 table2 joinOn = 
  let table1Statement = show $ tableStatement table1
      table2Statement = show $ tableStatement table2
      table1JoinName = "\{table1Statement}.\"\{column1 joinOn}\""
      table2JoinName = "\{table1Statement}.\"\{column1 joinOn}\""
      subquery = "SELECT * FROM \{table1Statement} JOIN \{table2Statement} ON \{table1JoinName} = \{table2JoinName}"
  in  RT (Subquery "tmp" subquery) ((columns table1) ++ (columns table2))

mappingCastable : {cs : _} -> ColumnMapping cs (name, ty) => Castable ty
mappingCastable {cs = ((name, Evidence pt (MkColType Nullable pt)) :: xs)} @{(HereNul name x @{sc})} = CastMaybe @{ValueCastString {pt}}
mappingCastable {cs = ((name, Evidence pt (MkColType NonNullable pt)) :: xs)} @{(Here name ty @{sc})} = Cast @{ValueCastString {pt}}
mappingCastable {cs = (x :: xs)} @{(There y)} = mappingCastable {cs=xs} @{y}

export
allCastable : PostgresTable t => {n : _} -> (table : t) -> {cols : Vect n _} -> HasMappings table cols => All Castable (Builtin.snd <$> cols)
allCastable @{_} _ {cols = []} @{[]} = []
allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} with (mappingCastable @{y})
  allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} | prf = prf :: allCastable @{_} _ {cols=xs} @{ys}

