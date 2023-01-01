import Postgres
import Postgres.Data.PostgresTable
import TestLib

table1 : PersistedTable
table1 = PT "table1" [
    ("id"    , col NonNullable PInteger)
  , ("field1", col NonNullable PDouble)
  ]

table2 : PersistedTable
table2 = PT "table2" [
    ("f_id"  , col NonNullable PInteger)
  , ("field2", col Nullable    PString)
  ]

table3 : PersistedTable
table3 = PT "table3" [
    ("f_id"  , col NonNullable PInteger)
  , ("field3", col NonNullable PString)
  ]

doubleJoin : RuntimeTable
doubleJoin = (innerJoin
               (innerJoin table1 table3 (on "id" "f_id"))
               table2 (on "table1.id" "f_id")
             )

-- we'll test the query compiles and then dump the select string value in the main function to compare golden values
query1 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String, String])))
query1 = tableQuery
           doubleJoin
           [("table1.field1", Double), ("table2.field2", Maybe String), ("table3.field3", String)]

main : IO ()
main = do
  putStrLn "double-join select statement: "
  putStrLn $ select doubleJoin [("table1.field1", Double), ("table2.field2", Maybe String), ("table3.field3", String)]

