module Postgres.Data.PostgresTable

import Postgres.Data.PostgresType
import public Data.DPair

import Postgres.Data.PostgresValue
import Data.List
import Data.Vect
import Data.Vect.Elem
import public Data.Vect.Quantifiers
import Data.String.Extra

%default total

public export
interface PostgresTable t where
  ||| A table name or subselect statement from which the following columns could be selected.
  tableStatement : t -> String
  ||| The columns this table offers.
  columns : t -> List (String, Exists PColType)

public export
record RuntimeTable where
  constructor RT
  tableStatement : String
  columns : List (String, Exists PColType)

export
implementation PostgresTable RuntimeTable where
  tableStatement = .tableStatement
  columns = .columns

public export
col : (nullable : Bool) -> (pt : PType) -> Exists PColType
col nullable pt = Evidence pt (MkColType nullable pt)

public export
data ColumnMapping : List (String, Exists PColType) -> (String, Type) -> Type where
  HereNul : (name : String) -> (ty : Type) -> SafeCast (PValue pt) ty => ColumnMapping ((name, (Evidence pt (MkColType True pt))) :: xs) (name, Maybe ty)
  Here : (name : String) -> (ty : Type) -> SafeCast (PValue pt) ty => ColumnMapping ((name, (Evidence pt (MkColType False pt))) :: xs) (name, ty)
  There : ColumnMapping xs (name, ty) -> ColumnMapping (x :: xs) (name, ty)

public export
HasMappings : PostgresTable t => (table : t) -> (cols : Vect n (String, Type)) -> Type
HasMappings table cols = All (ColumnMapping (columns table)) cols

export
select : PostgresTable t => (table : t) -> (cols : Vect n (String, Type)) -> (0 _ : HasMappings table cols) => String
select table cols =
  let tableStatement = tableStatement table
      columnNames    = join "," $ fst <$> (toList cols)
  in  "SELECT \{columnNames} FROM \"\{tableStatement}\""

hmm2 : String
hmm2 = select (RT "greetings" [("hello", col True PString), ("world", col False PInteger)]) [("hello", Maybe String), ("world", Integer)]

mappingCastable : {cs : _} -> ColumnMapping cs (name, ty) => Castable ty
mappingCastable {cs = ((name, Evidence pt (MkColType True pt)) :: xs)} @{(HereNul name x @{sc})} = CastMaybe @{SafeCastString {t1=pt}}
mappingCastable {cs = ((name, Evidence pt (MkColType False pt)) :: xs)} @{(Here name ty @{sc})} = Cast @{SafeCastString {t1=pt}}
mappingCastable {cs = (x :: xs)} @{(There y)} = mappingCastable {cs=xs} @{y}

export
allCastable : PostgresTable t => {n : _} -> (table : t) -> {cols : Vect n _} -> HasMappings table cols => All Castable (Prelude.map Builtin.snd cols)
allCastable @{_} _ {cols = []} @{[]} = []
allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} with (mappingCastable @{y})
  allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} | prf = prf :: allCastable @{_} _ {cols=xs} @{ys}

