module Postgres.Data.PostgresTable

import Postgres.Data.PostgresType
import public Data.DPair

import Postgres.Data.PostgresValue
import Data.List
import public Data.List1
import public Data.List.Elem
import Data.HVect
import Data.Vect
import Data.Vect.Elem
import public Data.Vect.Quantifiers
import public Data.String
import public Data.String.Extra

%default total

public export
data Ident = Id String

%name Ident id, id2, id3

export
Show Ident where
  show (Id name) = "\"\{name}\""

public export
data Alias = Named String
           | Generated Nat

export
aliasIdentifier : Alias -> Ident
aliasIdentifier (Named str) = Id str
aliasIdentifier (Generated k) = Id "_idr_t_\{show k}"

export
Show Alias where
  show = show . aliasIdentifier

public export
toAlias : Ident -> Alias
toAlias (Id str) = Named str

||| Show an AS statement fragment for the given
||| alias.
||| For a table alias of myTable this will be the string
||| literal (including double-quotes and leading space but
||| excluding single quotes): ' AS "myTable"'
maybeShowAsAlias : Maybe Alias -> String
maybeShowAsAlias Nothing = ""
maybeShowAsAlias (Just a) = " AS \{show a}"

||| Show an alias prefix for a fully qualified column.
||| For a table aliased as myTable this will be the string
||| literal (including double-quotes and trailing dot but
||| excluding single quotes): '"myTable".'
maybeShowAliasColumnPrefix : Maybe Alias -> String
maybeShowAliasColumnPrefix Nothing = ""
maybeShowAliasColumnPrefix (Just a) = "\{show a}."

||| A table name or subquery.
public export
data TableStatement = ||| A table name.
                      Identifier String (Maybe Alias)
                    | ||| A subquery.
                      Subquery String Alias
                    | ||| A fragment (can be selected against, but is not named or aliased)
                      Fragment String

public export
tableAlias : TableStatement -> Maybe Alias
tableAlias (Identifier str a) = a
tableAlias (Subquery str a) = Just a
tableAlias (Fragment str) = Nothing

||| The table alias or name.
public export
tableAliasOrName : TableStatement -> Maybe Ident
tableAliasOrName (Identifier name alias) = Just $ maybe (Id name) aliasIdentifier alias
tableAliasOrName (Subquery _ alias) = Just $ aliasIdentifier alias
tableAliasOrName (Fragment _) = Nothing

export
Show TableStatement where
  show (Identifier n a) = "\{show $ Id n}\{maybeShowAsAlias a}"
  show (Subquery query a) = "(\{query}) AS \{show a}"
  show (Fragment query) = query

||| Construct a table statement for a table with the given name.
public export
named : String -> {default Nothing alias : Maybe Alias} -> TableStatement
named str {alias} = Identifier str alias

||| Construct a table statement using a subquery.
export
subquery : String -> Alias -> TableStatement
subquery = Subquery

public export
record ColumnIdentifier where
  constructor MkColumnId
  sourceTable : Maybe Alias
  name : String

public export
replaceSource : Maybe Alias -> ColumnIdentifier -> ColumnIdentifier
replaceSource newSource (MkColumnId _ name) = MkColumnId newSource name

public export
forgetSource : ColumnIdentifier -> ColumnIdentifier
forgetSource = replaceSource Nothing

public export
data AnonymousColumnIdentifier : ColumnIdentifier -> Type where
  Anon : AnonymousColumnIdentifier (MkColumnId Nothing _)

public export
AllAnonymous : Vect n ColumnIdentifier -> Type
AllAnonymous xs = All AnonymousColumnIdentifier xs

export
Show ColumnIdentifier where
  show (MkColumnId sourceTable name) = "\{maybeShowAliasColumnPrefix sourceTable}\{show . Id $ name}"

public export
FromString ColumnIdentifier where
  fromString str =
    case (split (== '.') str) of
      (column ::: []) => MkColumnId Nothing column
      (table ::: column@(x :: xs)) => MkColumnId (Just $ Named table) (join "." column)
      -- ^ not great, just chooses not to handle x.y.z very well at all

||| Some representation of a Postgres table.
public export
interface PostgresTable t where
  ||| A table name or subselect statement from which the following columns could be selected.
  tableStatement : t -> TableStatement
  ||| The columns this table offers. Column names should not include double quotes, even where they
  ||| are needed when written down in SQL statements.
  columns : t -> List (ColumnIdentifier, Exists PColType)

export
alias : PostgresTable t => (table : t) -> Maybe Alias
alias = tableAlias . tableStatement

public export
aliasOrName : PostgresTable t => (table : t) -> Maybe Alias
aliasOrName = map toAlias . tableAliasOrName . tableStatement

||| A runtime representation of a table. These have string fields and table statements so as to not limit
||| themselves to nominal postgres tables; you can also represent joins naturally where the table statement is
||| a SELECT with a JOIN instead of just a table's name.
public export
record RuntimeTable where
  constructor RT
  tableStatement : TableStatement
  columns : List (ColumnIdentifier, Exists PColType)

export
PostgresTable RuntimeTable where
  tableStatement = .tableStatement
  columns = .columns

||| A persisted table is an actual named table in the database.
public export
record PersistedTable where
  constructor PT
  tableName : String
  {default Nothing alias : Maybe Alias}
  columns : List (String, Exists PColType)

namespace PersistedTable
  public export
  aliasOrName : (table : PersistedTable) -> Alias
  aliasOrName (PT tableName {alias} columns) = maybe (toAlias $ Id tableName) id alias

export
PostgresTable PersistedTable where
  columns table = mapFst (MkColumnId (Just . aliasOrName $ table)) <$> table.columns

  tableStatement (PT n {alias} _) = named n {alias}

public export
col : (nullable : Nullability) -> (pt : PType) -> Exists PColType
col nullable pt = Evidence pt (MkColType nullable pt)

||| A mapping between a column name and Idris type to some element in a list of column identifiers
||| and Postgres types. This mapping proves that the column identifiers exists and that the Postgres
||| type for that column can be cast to the Idris type specified.
public export
data ColumnMapping : (0 _ : PType -> Type -> Type) -> List (ColumnIdentifier, Exists PColType) -> (ColumnIdentifier, Type) -> Type where
  HereNul : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((ident, (Evidence pt (MkColType Nullable pt))) :: xs) (ident, Maybe ty)
  Here : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((ident, (Evidence pt (MkColType NonNullable pt))) :: xs) (ident, ty)
  There : ColumnMapping castTy xs (ident, ty) -> ColumnMapping castTy (x :: xs) (ident, ty)

Uninhabited (ColumnMapping _ [] _) where
  uninhabited (HereNul _ _) impossible
  uninhabited (Here _ _) impossible
  uninhabited (There _) impossible

public export
HasMappings : {n : _} -> (0 castTy : PType -> Type -> Type) -> PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> Type
HasMappings castTy table cols = All (ColumnMapping castTy (columns table)) cols

toString : ColumnMapping PGCast table (colName, colType) -> colType -> String
toString (HereNul name ty @{prf}) = \case Nothing => "null"; Just x => rawString $ toPostgres @{prf} x
toString (Here name ty @{prf}) = rawString . toPostgres @{prf}
toString (There y) = toString y

||| Create a select statement based on the columns you would like to grab from the
||| given table.
public export
select : PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> (0 _ : HasMappings IdrCast table cols) => String
select table cols =
  let tableStatement = show $ tableStatement table
      columnNames    = join "," $ show . fst <$> (toList cols)
  in  "SELECT \{columnNames} FROM \{tableStatement}"

namespace StringColumns
  ||| Create a select statement based on the columns you would like to grab from the
  ||| given table.
  public export
  select' : PostgresTable t => (table : t) -> (cols : Vect n (String, Type)) -> (0 _ : HasMappings IdrCast table (mapFst (MkColumnId $ aliasOrName table) <$> cols)) => String
  select' table cols = select table (mapFst (MkColumnId $ aliasOrName table) <$> cols)

public export
HasInsertMappings : {n : _} -> (0 castTy : PType -> Type -> Type) -> (table : PersistedTable) -> (cols : Vect n ColumnIdentifier) -> (colTypes : Vect n Type) -> Type
HasInsertMappings castTy table cols colTypes = Either (HasMappings {n} castTy table (zip cols colTypes)) (AllAnonymous cols, HasMappings {n} castTy table (zip (replaceSource (aliasOrName table) <$> cols) colTypes))

||| Insert the given values into the given columns of a new row in the given table.
public export
insert : {n : _}
      -> (table : PersistedTable)
      -> (cols : Vect n ColumnIdentifier)
      -> {colTypes : Vect n Type}
      -> (values : HVect colTypes)
      -> HasInsertMappings PGCast table cols colTypes =>
         String
insert table cols vs @{mappings} =
  let tableIdentifier = show $ Id table.tableName
      columnNames     = '(' <+ (join "," $ show . .name <$> (toList cols)) +> ')'
      values : List String = case mappings of
                    (Left ms)  => values table cols vs
                    (Right (_, ms)) => values table (replaceSource (aliasOrName table) <$> cols) vs @{ms}
      valueStrings    = '(' <+ (join "," $ values) +> ')'
  in  "INSERT INTO \{tableIdentifier} \{columnNames} VALUES \{valueStrings}"
  where
    values : {l : _} -> (table : PersistedTable) -> (cols : Vect l ColumnIdentifier) -> {colTypes : Vect l Type} -> (values : HVect colTypes) -> HasMappings PGCast table (zip cols colTypes) => List String
    values table [] [] = []
    values table (x :: xs) (v :: vs) @{m :: ms} = toString m v :: values table xs vs @{ms}

namespace StringColumns
  ||| Insert the given values into the given columns of a new row in the given table.
  public export
  insert' : {n : _} -> (table : PersistedTable) -> (cols : Vect n String) -> {colTypes : Vect n Type} -> (values : HVect colTypes) -> HasMappings PGCast table (zip ((MkColumnId $ aliasOrName table) <$> cols) colTypes) => String
  insert' table cols vs = insert table ((MkColumnId $ aliasOrName table) <$> cols) vs

public export
data Join : t1 -> t2 -> Type where
  On : PostgresTable t => PostgresTable u => {t1 : t} -> {t2 : u} -> (c1 : ColumnIdentifier) -> (c2 : ColumnIdentifier) -> (0 _ : Elem c1 (Builtin.fst <$> (columns t1))) => (0 _ : Elem c2 (Builtin.fst <$> (columns t2))) => Join t1 t2

namespace Join
  public export
  column1 : Join t u -> ColumnIdentifier
  column1 (On c1 c2) = c1

  public export
  column2 : Join t u -> ColumnIdentifier
  column2 (On c1 c2) = c2

public export
maxGeneratedAliasId : Maybe Alias -> Maybe Alias -> Nat
maxGeneratedAliasId (Just (Generated k)) (Just (Generated j)) = max k j
maxGeneratedAliasId _                    (Just (Generated j)) = j
maxGeneratedAliasId (Just (Generated k)) _                    = k
maxGeneratedAliasId _ _ = 0

public export
generatedJoinAlias : PostgresTable t => PostgresTable u => t -> u -> Alias
generatedJoinAlias t u = Generated $ S $ maxGeneratedAliasId (alias t) (alias u)

||| Construct a runtime table by joining two other tables on a specified column.
public export
innerJoin : PostgresTable t => PostgresTable u => (table1 : t) -> (table2 : u) -> (on : Join table1 table2) -> RuntimeTable
innerJoin table1 table2 joinOn = 
  let table1Statement = show $ tableStatement table1
      table2Statement = show $ tableStatement table2
      table1JoinName = show $ column1 joinOn
      table2JoinName = show $ column2 joinOn
      subquery = "\{table1Statement} JOIN \{table2Statement} ON \{table1JoinName} = \{table2JoinName}"
      table1Cols = columns table1
      table2Cols = columns table2
  in  RT (Fragment subquery) (table1Cols ++ table2Cols)
  where
    generatedAlias : Alias
    generatedAlias = generatedJoinAlias table1 table2

public export 
leftJoin : PostgresTable t => PostgresTable u => (table1 : t) -> (table2 : u) -> (on : Join table1 table2) -> RuntimeTable
leftJoin table1 table2 joinOn =
  let table1Statement = show $ tableStatement table1
      table2Statement = show $ tableStatement table2
      table1JoinName = show $ column1 joinOn
      table2JoinName = show $ column2 joinOn
      subquery = "\{table1Statement} LEFT JOIN \{table2Statement} ON \{table1JoinName} = \{table2JoinName}"
      table1Cols = columns table1
      table2Cols = mapSnd (bimap (\t => t) makeNullable) <$> columns table2
      -- ^ need to make table2's columns possibly null because of the left-join.
  in  RT (Fragment subquery) (table1Cols ++ table2Cols)
  where
    generatedAlias : Alias
    generatedAlias = generatedJoinAlias table1 table2

mappingCastable : {cs : _} -> ColumnMapping IdrCast cs (ident, ty) => Castable ty
mappingCastable {cs = ((ident, Evidence pt (MkColType Nullable pt)) :: xs)} @{(HereNul ident x @{sc})} = CastMaybe @{IdrCastString {pt}}
mappingCastable {cs = ((ident, Evidence pt (MkColType NonNullable pt)) :: xs)} @{(Here ident ty @{sc})} = Cast @{IdrCastString {pt}}
mappingCastable {cs = (x :: xs)} @{(There y)} = mappingCastable {cs=xs} @{y}

export
allCastable : PostgresTable t => {n : _} -> (table : t) -> {cols : Vect n _} -> HasMappings IdrCast table cols => All Castable (Builtin.snd <$> cols)
allCastable @{_} _ {cols = []} @{[]} = []
allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} with (mappingCastable @{y})
  allCastable @{_} _ {cols = ((x, z) :: xs)} @{(y :: ys)} | prf = prf :: allCastable @{_} _ {cols=xs} @{ys}

