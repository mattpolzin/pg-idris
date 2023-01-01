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

export
Eq Alias where
  (Named str) == (Named str') = str == str'
  (Generated k) == (Generated k') = k == k'
  _ == _ = False

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
  ||| Set the alias on a table
  as : t -> String -> t

infix 0 `as`

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

public export
PostgresTable RuntimeTable where
  tableStatement = .tableStatement
  columns = .columns

  as (RT (Identifier str x) columns) a = RT (Identifier str (Just $ Named a)) (mapFst (replaceSource . Just $ Named a) <$> columns)
  as (RT (Subquery str x) columns) a = RT (Subquery str (Named a)) (mapFst (replaceSource . Just $ Named a) <$> columns)
  as (RT (Fragment str) columns) a = RT (Subquery "SELECT * FROM \{str}" (Named a)) (mapFst (replaceSource . Just $ Named a) <$> columns)

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

public export
PostgresTable PersistedTable where
  columns table = mapFst (MkColumnId (Just . aliasOrName $ table)) <$> table.columns

  tableStatement (PT n {alias} _) = named n {alias}

  as (PT tableName columns) a = PT tableName {alias=Just $ Named a} columns

public export
col : (nullable : Nullability) -> (pt : PType) -> Exists PColType
col nullable pt = Evidence pt (MkColType nullable pt)

||| A mapping between a column name and Idris type to some element in a list of column identifiers
||| and Postgres types. This mapping proves that the column identifiers exists and that the Postgres
||| type for that column can be cast to the Idris type specified. This mapping allows for "loose"
||| matches where the ColumnIdentifier being matched against does not specify a source table but the
||| column name can be found in the list of columns given. Loose matching is only ok if the match would
||| not succeed against multiple columns (Postgres will complain of ambiguity, for good reason). Use the
||| ColumnMapping type to avoid loose matches.
public export
data LooseColumnMapping : (0 _ : PType -> Type -> Type) -> List (ColumnIdentifier, Exists PColType) -> (ColumnIdentifier, Type) -> Type where
  HereNul       : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => LooseColumnMapping castTy ((ident, (Evidence pt (MkColType Nullable pt))) :: xs) (ident, Maybe ty)
  HereNulLoose  : (columnName : String) -> (ty : Type) -> castTy pt ty => LooseColumnMapping castTy ((MkColumnId _ columnName, (Evidence pt (MkColType Nullable pt))) :: xs) (MkColumnId Nothing columnName, Maybe ty)
  Here          : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => LooseColumnMapping castTy ((ident, (Evidence pt (MkColType NonNullable pt))) :: xs) (ident, ty)
  HereLoose     : (columnName : String) -> (ty : Type) -> castTy pt ty => LooseColumnMapping castTy ((MkColumnId _ columnName, (Evidence pt (MkColType NonNullable pt))) :: xs) (MkColumnId Nothing columnName, ty)
  There         : LooseColumnMapping castTy xs (ident, ty) -> LooseColumnMapping castTy (x :: xs) (ident, ty)

Uninhabited (LooseColumnMapping _ [] _) where
  uninhabited (HereNul _ _) impossible
  uninhabited (HereNulLoose _ _) impossible
  uninhabited (Here _ _) impossible
  uninhabited (HereLoose _ _) impossible
  uninhabited (There _) impossible

namespace Strict
  public export
  data ColumnMapping : (0 _ : PType -> Type -> Type) -> List (ColumnIdentifier, Exists PColType) -> (ColumnIdentifier, Type) -> Type where
    HereNul       : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((ident, (Evidence pt (MkColType Nullable pt))) :: xs) (ident, Maybe ty)
    Here          : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((ident, (Evidence pt (MkColType NonNullable pt))) :: xs) (ident, ty)
    There         : ColumnMapping castTy xs (ident, ty) -> ColumnMapping castTy (x :: xs) (ident, ty)

public export
sameColumnNoSourceId : (ColumnIdentifier, Type) -> (ColumnIdentifier, Exists PColType) -> Bool
sameColumnNoSourceId ((MkColumnId Nothing name), z) ((MkColumnId _ name'), w) = name == name'
sameColumnNoSourceId ((MkColumnId (Just _) _), _) _ = False

public export
data UniqueCol : List (ColumnIdentifier, Exists PColType) -> (ColumnIdentifier, Type) -> Type where
  IsUniqueCol : (xs : List (ColumnIdentifier, Exists PColType)) -> (y : (ColumnIdentifier, Type)) -> (So $ (count (sameColumnNoSourceId y) xs) == 1) -> UniqueCol xs y

public export
HasLooseMappings : {n : _} -> (0 castTy : PType -> Type -> Type) -> PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> Type
HasLooseMappings castTy table cols = All (LooseColumnMapping castTy (columns table)) cols

public export
HasMappings : {n : _} -> (0 castTy : PType -> Type -> Type) -> PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> Type
HasMappings castTy table cols = All (ColumnMapping castTy (columns table)) cols

toString : ColumnMapping PGCast table (colName, colType) -> colType -> String
toString (HereNul name ty @{prf}) = \case Nothing => "null"; Just x => rawString $ toPostgres @{prf} x
toString (Here name ty @{prf}) = rawString . toPostgres @{prf}
toString (There y) = toString y

public export
HasSelectMappings : {n : _} -> (0 castTy : PType -> Type -> Type) -> PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> Type
HasSelectMappings castTy table cols = Either (HasMappings IdrCast table cols) (All (UniqueCol (columns table)) cols, HasLooseMappings IdrCast table cols)

||| Create a select statement based on the columns you would like to grab from the
||| given table.
public export
select : PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> (0 _ : HasSelectMappings IdrCast table cols) => String
select table cols =
  let tableStatement = show $ tableStatement table
      columnNames    = join "," $ show . fst <$> (toList cols)
  in  "SELECT \{columnNames} FROM \{tableStatement}"

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

public export
data Join : t1 -> t2 -> Type where
  On : PostgresTable t => PostgresTable u => {t1 : t} -> {t2 : u} -> (c1 : ColumnIdentifier) -> (c2 : ColumnIdentifier) -> (0 _ : Elem c1 (Builtin.fst <$> (columns t1))) => (0 _ : Elem c2 (Builtin.fst <$> (columns t2))) => Join t1 t2

public export
HasOnMapping : ColumnIdentifier -> PostgresTable t =>  (table : t) -> Type
HasOnMapping c table = Either (Elem c (fst <$> (columns table))) (AnonymousColumnIdentifier c, Elem (replaceSource (aliasOrName table) c) (fst <$> (columns table)))

public export
elemForOnMapping : PostgresTable t => (table : t) -> (c : ColumnIdentifier) -> HasOnMapping c table -> (c' ** Elem c' (Builtin.fst <$> (columns table)))
elemForOnMapping table c (Left m) = (c ** m)
elemForOnMapping table c (Right (_, m)) = (replaceSource (aliasOrName table) c ** m)

public export
on : PostgresTable t => PostgresTable u => {t1 : t} -> {t2 : u} -> (c1 : ColumnIdentifier) -> (c2 : ColumnIdentifier) -> {auto on1 : HasOnMapping c1 t1} -> {auto on2 : HasOnMapping c2 t2} -> Join t1 t2
on c1 c2 {on1} {on2} with (elemForOnMapping t1 c1 on1, elemForOnMapping t2 c2 on2)
  on c1 c2 {on1} {on2} | ((c1' ** on1'), (c2' ** on2')) = On c1' c2'

public export
(==) : PostgresTable t => PostgresTable u => {t1 : t} -> {t2 : u} -> (c1 : ColumnIdentifier) -> (c2 : ColumnIdentifier) -> {auto on1 : HasOnMapping c1 t1} -> {auto on2 : HasOnMapping c2 t2} -> Join t1 t2
(==) = on

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

mappingCastable' : {cs : _} -> LooseColumnMapping IdrCast cs (ident, ty) => Castable ty
mappingCastable' {cs = ((ident, Evidence pt (MkColType Nullable pt)) :: xs)} @{(HereNul ident x @{sc})} = CastMaybe @{IdrCastString {pt}}
mappingCastable' {cs = ((MkColumnId _ n, Evidence pt (MkColType Nullable pt)) :: xs)} @{(HereNulLoose n x @{sc})} = CastMaybe @{IdrCastString {pt}}
mappingCastable' {cs = ((ident, Evidence pt (MkColType NonNullable pt)) :: xs)} @{(Here ident ty @{sc})} = Cast @{IdrCastString {pt}}
mappingCastable' {cs = ((MkColumnId _ n, Evidence pt (MkColType NonNullable pt)) :: xs)} @{(HereLoose n ty @{sc})} = Cast @{IdrCastString {pt}}
mappingCastable' {cs = (x :: xs)} @{(There y)} = mappingCastable' {cs=xs} @{y}

export
allCastable : PostgresTable t => {n : _} -> (table : t) -> {cols : Vect n _} -> HasSelectMappings IdrCast table cols => All Castable (Builtin.snd <$> cols)
allCastable @{_} _ {cols = []} @{Left []} = []
allCastable @{_} _ {cols = []} @{Right (_, [])} = []
allCastable @{_} _ {cols = ((x, z) :: xs)} @{Left (y :: ys)} with (mappingCastable @{y})
  allCastable @{_} _ {cols = ((x, z) :: xs)} @{Left (y :: ys)} | prf = prf :: allCastable @{_} _ {cols=xs} @{Left ys}
allCastable @{_} _ {cols = ((x, z) :: xs)} @{Right (m, (y :: ys))} with (mappingCastable' @{y})
  allCastable @{_} _ {cols = ((x, z) :: xs)} @{Right ((m :: ms), (y :: ys))} | prf = prf :: allCastable @{_} _ {cols=xs} @{Right (ms, ys)}

