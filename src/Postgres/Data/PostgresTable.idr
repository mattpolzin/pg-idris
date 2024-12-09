module Postgres.Data.PostgresTable

import Postgres.Data.PostgresType
import public Data.DPair

import Postgres.Data.PostgresValue
import Data.List
import public Data.List1
import public Data.List.Elem
import Data.Vect
import Data.Vect.Elem
import public Data.Vect.Quantifiers
import public Data.String

%default total

||| A Postgres identifier, generally speaking. Could be a table name
||| or a column name or more broadly speaking any proper name that
||| Postgres syntactically recognizes between double-quotes.
public export
data Ident = Id String

%name Ident id, id2, id3

export
Show Ident where
  show (Id name) = "\"\{name}\""

||| An Alias is a valid table identifier that may either
||| be a string specified by the programmer or it may be
||| auto-generated in the course of creating subqueries.
public export
data Alias = Named String
           | Generated Nat

%name Alias alias, alias1, alias2

||| Aliases can always be turned into identifiers.
public export
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

||| Turning an identifier into an alias is not inteded to be the lossless
||| reversal of `aliasIdentifier`. An identifier will always become a named
||| alias even if it was at one point a generated alias before being turned into
||| an identifier.
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

||| A column is identified by its name and optionally the alias
||| of a table the column comes from. In some situations the `sourceTable`
||| for a column will be needed in order to unambiguously refer to one
||| of a few columns in the same query with the same name.
|||
||| Generally it will be clunky to create ColumnIdentifiers using the type's
||| consutrctor directly so most column identifiers will come from string values
||| because ColumnIdentifier's `FromString` conformance allows Idris 2 to transform
||| strings into ColumnIdentifiers. This allows for:
||| - "table1.column1" = MkColumnId (Just $ Named "table1") "column1"
||| - "column1" = MkColumnId Nothing "column1"
|||
||| Table aliases here do not necessarily need to refer to a table that has
||| been given a Postgres alias (a la "(select ...) AS an_alias"). A normal table
||| or view name is appropriate and can be turned into an alias for use in a column
||| identifier with the `toAlias` function.
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

export
Show ColumnIdentifier where
  show (MkColumnId sourceTable name) = "\{maybeShowAliasColumnPrefix sourceTable}\{show . Id $ name}"

public export
FromString ColumnIdentifier where
  fromString str =
    case (split (== '.') str) of
      (column ::: []) => MkColumnId Nothing column 
      (table ::: column@(x :: xs)) => MkColumnId (Just $ Named table) (joinBy "." column)
      -- ^ not great, just chooses not to handle x.y.z very well at all

||| Some representation of a Postgres table.
public export
interface PostgresTable t where
  ||| A table name or subselect statement from which the following columns could be selected.
  tableStatement : t -> TableStatement
  ||| The columns this table offers. Column names should not include double quotes, even where they
  ||| are needed when written down in SQL statements.
  columns : t -> List (ColumnIdentifier, Exists PColType)
  ||| Set the alias on a table.
  as : t -> String -> t

infix 0 `as`

export
alias : PostgresTable t => (table : t) -> Maybe Alias
alias = tableAlias . tableStatement

||| Get the alias or name of a table; prefer the alias, as a renamed table
||| _should_ be referred to by its alias, but use the table's original name
||| if one is available and there is no alias set.
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

||| A persisted table is an actual named (or view) table in the database.
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

||| Construct an existential column type from a Postgres type and its
||| nullability.
public export
col : (nullable : Nullability) -> (pt : PType) -> Exists PColType
col nullable pt = Evidence pt (MkColType nullable pt)

||| Construct a persisted table representation (a table name and the names,
||| types, and nullabilities of the columns of a table.)
|||
||| ```idris example
||| myTable = table "table1" [ ("id"            , NonNullable, PInteger)
|||                          , ("name"          , NonNullable, PString)
|||                          , ("favorite_color", Nullable   , PString)
|||                          ]
||| ```
public export
pgTable : (name : String) -> (columns : List (String, Nullability, PType)) -> PersistedTable
pgTable name columns = PT name (mapSnd (uncurry col) <$> columns)

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

public export
HasLooseMappings : {n : _} -> (0 castTy : PType -> Type -> Type) -> PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> Type
HasLooseMappings castTy table cols = All (LooseColumnMapping castTy (columns table)) cols

namespace Strict
  ||| A mapping between a column name and Idris type to some element in a list of column identifiers
  ||| and Postgres types. This mapping proves that the column identifiers exists and that the Postgres
  ||| type for that column can be cast to the Idris type specified.
  public export
  data ColumnMapping : (0 _ : PType -> Type -> Type) -> List (ColumnIdentifier, Exists PColType) -> (ColumnIdentifier, Type) -> Type where
    HereNul       : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((ident, (Evidence pt (MkColType Nullable pt))) :: xs) (ident, Maybe ty)
    Here          : (ident : ColumnIdentifier) -> (ty : Type) -> castTy pt ty => ColumnMapping castTy ((ident, (Evidence pt (MkColType NonNullable pt))) :: xs) (ident, ty)
    There         : ColumnMapping castTy xs (ident, ty) -> ColumnMapping castTy (x :: xs) (ident, ty)

  public export
  HasMappings : {n : _} -> (0 castTy : PType -> Type -> Type) -> PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> Type
  HasMappings castTy table cols = All (ColumnMapping castTy (columns table)) cols

toString : LooseColumnMapping PGCast table (colName, colType) -> colType -> String
toString (HereNul name ty @{prf}) = \case Nothing => "null"; Just x => rawString $ toPostgres @{prf} x
toString (HereNulLoose name ty @{prf}) = \case Nothing => "null"; Just x => rawString $ toPostgres @{prf} x
toString (Here name ty @{prf}) = rawString . toPostgres @{prf}
toString (HereLoose name ty @{prf}) = rawString . toPostgres @{prf}
toString (There y) = toString y

||| Create a select statement based on the columns you would like to grab from the
||| given table.
public export
select : PostgresTable t => (table : t) -> (cols : Vect n (ColumnIdentifier, Type)) -> (0 _ : HasMappings IdrCast table cols) => String
select table cols =
  let tableStatement = show $ tableStatement table
      columnNames    = joinBy "," $ show . fst <$> (toList cols)
  in  "SELECT \{columnNames} FROM \{tableStatement}"

||| Insert the given values into the given columns of a new row in the given table.
public export
insert : {n : _}
      -> (table : PersistedTable)
      -> (cols : Vect n ColumnIdentifier)
      -> {colTypes : Vect n Type}
      -> (values : HVect colTypes)
      -> HasLooseMappings PGCast table (zip cols colTypes) =>
         String
insert table cols vs @{mappings} =
  let tableIdentifier = show $ Id table.tableName
      columnNames     = (strCons '(' (joinBy "," $ show . .name <$> (toList cols))) ++ (singleton ')')
      values          = values table cols vs
      valueStrings    = (strCons '(' (joinBy "," $ values)) ++ (singleton ')')
  in  "INSERT INTO \{tableIdentifier} \{columnNames} VALUES \{valueStrings}"
  where
    values : {l : _} -> (table : PersistedTable) -> (cols : Vect l ColumnIdentifier) -> {colTypes : Vect l Type} -> (values : HVect colTypes) -> HasLooseMappings PGCast table (zip cols colTypes) => List String
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

||| In a join-statement, produce the clause that links the two tables together by
||| a shared column. In pseudo-code: "table1 join table2 'on' col1 = col2".
|||
||| ```idris example
||| innerJoin' table1 table2 ("col_from_table1" == "col_from_table2")
||| ```
public export
(==) : PostgresTable t => PostgresTable u => {t1 : t} -> {t2 : u} -> (c1 : ColumnIdentifier) -> (c2 : ColumnIdentifier) -> {auto on1 : HasOnMapping c1 t1} -> {auto on2 : HasOnMapping c2 t2} -> Join t1 t2
(==) c1 c2 {on1} {on2} with (elemForOnMapping t1 c1 on1, elemForOnMapping t2 c2 on2)
  (==) c1 c2 {on1} {on2} | ((c1' ** on1'), (c2' ** on2')) = On c1' c2'

namespace Join
  public export
  column1 : Join t u -> ColumnIdentifier
  column1 (On c1 c2) = c1

  public export
  column2 : Join t u -> ColumnIdentifier
  column2 (On c1 c2) = c2

||| Construct a runtime table by inner-joining two other tables on a specified column.
|||
||| ```idris example
||| innerJoin' table1 table2 ("col_from_table1" == "col_from_table2")
||| ```
public export
innerJoin' : PostgresTable t => (table1 : t) -> PostgresTable u => (table2 : u) -> (on : Join table1 table2) -> RuntimeTable
innerJoin' table1 table2 joinOn = 
  let table1Statement = show $ tableStatement table1
      table2Statement = show $ tableStatement table2
      table1JoinName = show $ column1 joinOn
      table2JoinName = show $ column2 joinOn
      subquery = "\{table1Statement} JOIN \{table2Statement} ON \{table1JoinName} = \{table2JoinName}"
      table1Cols = columns table1
      table2Cols = columns table2
  in  RT (Fragment subquery) (table1Cols ++ table2Cols)

||| Construct a runtime table by left-joining two other tables on a specified column.
|||
||| ```idris example
||| leftJoin' table1 table2 ("col_from_table1" == "col_from_table2")
||| ```
public export 
leftJoin' : PostgresTable t => (table1 : t) -> PostgresTable u => (table2 : u) -> (on : Join table1 table2) -> RuntimeTable
leftJoin' table1 table2 joinOn =
  let table1Statement = show $ tableStatement table1
      table2Statement = show $ tableStatement table2
      table1JoinName = show $ column1 joinOn
      table2JoinName = show $ column2 joinOn
      subquery = "\{table1Statement} LEFT JOIN \{table2Statement} ON \{table1JoinName} = \{table2JoinName}"
      table1Cols = columns table1
      table2Cols = mapSnd (bimap (\t => t) makeNullable) <$> columns table2
      -- ^ need to make table2's columns possibly null because of the left-join.
  in  RT (Fragment subquery) (table1Cols ++ table2Cols)

||| Construct a runtime table by right-joining two other tables on a specified column.
|||
||| ```idris example
||| rightJoin' table1 table2 ("col_from_table1" == "col_from_table2")
||| ```
public export 
rightJoin' : PostgresTable t => (table1 : t) -> PostgresTable u => (table2 : u) -> (on : Join table1 table2) -> RuntimeTable
rightJoin' table1 table2 joinOn =
  let table1Statement = show $ tableStatement table1
      table2Statement = show $ tableStatement table2
      table1JoinName = show $ column1 joinOn
      table2JoinName = show $ column2 joinOn
      subquery = "\{table1Statement} LEFT JOIN \{table2Statement} ON \{table1JoinName} = \{table2JoinName}"
      table1Cols = mapSnd (bimap (\t => t) makeNullable) <$> columns table1
      -- ^ need to make table1's columns possibly null because of the right-join.
      table2Cols = columns table2
  in  RT (Fragment subquery) (table1Cols ++ table2Cols)

public export
data JoinType = Inner | Left | Right

public export
data TablePair : table1 -> table2 -> Type where
  MkTP : JoinType -> PostgresTable t => PostgresTable u => (table1 : t) -> (table2 : u) -> TablePair table1 table2

||| Inner-join two tables.
||| This produces a pair of tables and a join strategy. You then need to use `onColumns` to take the table pair
||| and add the columns that the join should occur over.
public export
innerJoin : PostgresTable t => PostgresTable u => (table1 : t) -> (table2 : u) -> TablePair table1 table2
innerJoin = MkTP Inner

||| Left-join two tables.
||| This produces a pair of tables and a join strategy. You then need to use `onColumns` to take the table pair
||| and add the columns that the join should occur over.
public export
leftJoin : PostgresTable t => PostgresTable u => (table1 : t) -> (table2 : u) -> TablePair table1 table2
leftJoin = MkTP Left

||| Right-join two tables.
||| This produces a pair of tables and a join strategy. You then need to use `onColumns` to take the table pair
||| and add the columns that the join should occur over.
public export
rightJoin : PostgresTable t => PostgresTable u => (table1 : t) -> (table2 : u) -> TablePair table1 table2
rightJoin = MkTP Right

||| Given that you've already paired two tables up (see `innerJoin`, `leftJoin`, `rightJoin`)
||| you use `onColumns` to specify which columns from each table in the pair should be equal
||| in the joined table. The end result is a new table.
|||
||| Psuedo-example:
|||   table1 `innerJoin` table2 `onColumns` ("table1.col1" == "table2.col2")
public export
onColumns : TablePair table1 table2 -> Join table1 table2 -> RuntimeTable
onColumns (MkTP Inner table1 table2) joinOn = innerJoin' table1 table2 joinOn
onColumns (MkTP Left table1 table2) joinOn = leftJoin' table1 table2 joinOn
onColumns (MkTP Right table1 table2) joinOn = rightJoin' table1 table2 joinOn

infixl 10 `innerJoin`, `leftJoin`, `rightJoin`, `onColumns`

mappingCastable : {cs : _} -> ColumnMapping IdrCast cs (ident, ty) => Castable ty
mappingCastable {cs = ((ident, Evidence pt (MkColType Nullable pt)) :: xs)} @{(HereNul ident x @{sc})} = CastMaybe @{IdrCastString {pt}}
mappingCastable {cs = ((ident, Evidence pt (MkColType NonNullable pt)) :: xs)} @{(Here ident ty @{sc})} = Cast @{IdrCastString {pt}}
mappingCastable {cs = (x :: xs)} @{(There y)} = mappingCastable {cs=xs} @{y}

export
allCastable : PostgresTable t => {n : _} -> (table : t) -> {cols : Vect n _} -> HasMappings IdrCast table cols => All Castable (Builtin.snd <$> cols)
allCastable @{_} table {cols = []} @{[]} = []
allCastable @{_} table {cols = ((x, y) :: xs)} @{(m :: ms)} = (mappingCastable @{m}) :: allCastable table {cols=xs}

