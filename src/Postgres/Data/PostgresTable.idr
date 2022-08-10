module Postgres.Data.PostgresTable

import Postgres.Data.PostgresType
import public Data.DPair

import Postgres.Data.PostgresValue
import Data.List
import public Data.List.Elem
import Data.HVect
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
PostgresTable RuntimeTable where
  tableStatement = .tableStatement
  columns = .columns

||| A persisted table is an actual named table in the database.
public export
record PersistedTable where
  constructor PT
  tableName : String
  columns : List (String, Exists PColType)

export
PostgresTable PersistedTable where
  tableStatement = Identifier . tableName
  columns = .columns

public export
col : (nullable : Nullability) -> (pt : PType) -> Exists PColType
col nullable pt = Evidence pt (MkColType nullable pt)

||| A mapping between a column name and Idris type to some element in a list of column names
||| and Postgres types. This mapping proves that the column names exists and that the Postgres
||| type for that column can be cast to the Idris type specified.
public export
data ColumnMapping : (0 _ : PType -> Type -> Type) -> List (String, Exists PColType) -> (String, Type) -> Type where
  HereNul : (name : String) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((name, (Evidence pt (MkColType Nullable pt))) :: xs) (name, Maybe ty)
  Here : (name : String) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((name, (Evidence pt (MkColType NonNullable pt))) :: xs) (name, ty)
  There : ColumnMapping castTy xs (name, ty) -> ColumnMapping castTy (x :: xs) (name, ty)

Uninhabited (ColumnMapping _ [] _) where
  uninhabited (HereNul _ _) impossible
  uninhabited (Here _ _) impossible
  uninhabited (There _) impossible

public export
HasMappings : (0 castTy : PType -> Type -> Type) -> PostgresTable t => (table : t) -> (cols : Vect n (String, Type)) -> Type
HasMappings castTy table cols = All (ColumnMapping castTy (columns table)) cols

toString : ColumnMapping PGCast table (colName, colType) -> colType -> String
toString (HereNul name ty @{prf}) = \case Nothing => "null"; Just x => rawString $ toPostgres @{prf} x
toString (Here name ty @{prf}) = rawString . toPostgres @{prf}
toString (There y) = toString y

||| Create a select statement based on the columns you would like to grab from the
||| given table.
public export
select : PostgresTable t => (table : t) -> (cols : Vect n (String, Type)) -> (0 _ : HasMappings IdrCast table cols) => String
select table cols =
  let tableStatement = show $ tableStatement table
      columnNames    = join "," $ show . Identifier . fst <$> (toList cols)
  in  "SELECT \{columnNames} FROM \{tableStatement}"

public export
insert : {n : _} -> (table : PersistedTable) -> (cols : Vect n String) -> {colTypes : Vect n Type} -> (values : HVect colTypes) -> HasMappings PGCast table (zip cols colTypes) => String
insert table cols vs =
  let tableIdentifier = show $ Identifier table.tableName
      columnNames     = '(' <+ (join "," $ show . Identifier <$> (toList cols)) +> ')'
      valueStrings    = '(' <+ (join "," $ values table cols vs) +> ')'
  in  "INSERT INTO \{tableIdentifier} \{columnNames} VALUES \{valueStrings}"
  where
    values : {n : _} -> (table : PersistedTable) -> (cols : Vect n String) -> {colTypes : Vect n Type} -> (values : HVect colTypes) -> HasMappings PGCast table (zip cols colTypes) => List String
    values table [] {colTypes = []} vs @{mappings} = []
    values table (x :: xs) {colTypes = (y :: ys)} (v :: vs) @{(m :: ms)} = toString m v :: values table xs {colTypes=ys} vs @{ms}

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

mappingCastable : {cs : _} -> ColumnMapping IdrCast cs (name, ty) => Castable ty
mappingCastable {cs = ((name, Evidence pt (MkColType Nullable pt)) :: xs)} @{(HereNul name x @{sc})} = CastMaybe @{IdrCastString {pt}}
mappingCastable {cs = ((name, Evidence pt (MkColType NonNullable pt)) :: xs)} @{(Here name ty @{sc})} = Cast @{IdrCastString {pt}}
mappingCastable {cs = (x :: xs)} @{(There y)} = mappingCastable {cs=xs} @{y}

export
allCastable : PostgresTable t => {n : _} -> (table : t) -> {cols : Vect n _} -> HasMappings IdrCast table cols => All Castable (Builtin.snd <$> cols)
allCastable @{_} _ {cols = []} @{[]} = []
allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} with (mappingCastable @{y})
  allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} | prf = prf :: allCastable @{_} _ {cols=xs} @{ys}

