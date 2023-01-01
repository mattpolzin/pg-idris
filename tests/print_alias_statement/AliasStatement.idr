import Postgres
import Postgres.Data.PostgresTable
import TestLib

table1 : PersistedTable
table1 = PT "table1" [
    ("i", col NonNullable PInteger)
  , ("d", col NonNullable PDouble)
  , ("b", col NonNullable PBoolean)
  , ("t", col NonNullable PString)
  , ("c", col NonNullable PChar)
  , ("j", col NonNullable PJson)
  , ("ai", col NonNullable (PArray PInteger))
  , ("dm", col Nullable PDouble)
  ]

table2 : PersistedTable
table2 = PT "table2" [
    ("f_i", col NonNullable PInteger)
  , ("extra1", col NonNullable PString)
  , ("extra2", col NonNullable PString)
  ]

testQuery : Vect ? (ColumnIdentifier, Type)
testQuery = [
    ("out.i", Integer)
  , ("out.d", Double)
  , ("out.b", Bool)
  , ("out.t", String)
  , ("out.c", Char)
  , ("out.j", JSON)
  , ("out.ai", (List Integer))
  , ("out.dm", Maybe Double)
  , ("out.extra1", Maybe String)
  , ("out.extra2", Maybe String)
  ]

-- we'll test the query compiles and then dump the select string value in the main function to compare golden values
query1 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
query1 = tableQuery
           (leftJoin (table1 `as` "t") (table2 `as` "o") (On "t.i" "o.f_i") `as` "out")
           testQuery

main : IO ()
main = do
  putStrLn "lots of alias in select statement: "
  putStrLn $ select (leftJoin (table1 `as` "t") (table2 `as` "o") (On "t.i" "o.f_i") `as` "out") testQuery

