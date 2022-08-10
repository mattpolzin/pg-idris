import Postgres
import Postgres.Data.PostgresTable
import TestLib

table1 : RuntimeTable
table1 = RT (named "table1") [
    ("id"    , col NonNullable PInteger)
  , ("field1", col NonNullable PDouble)
  ]

table2 : RuntimeTable
table2 = RT (named "table2") [
    ("f_id"  , col NonNullable PInteger)
  , ("field2", col Nullable    PString)
  ]

table3 : RuntimeTable
table3 = RT (named "table3") [
    ("f_id"  , col NonNullable PInteger)
  , ("field3", col NonNullable PString)
  ]

doubleJoin : RuntimeTable
doubleJoin = (innerJoin
               (innerJoin table1 table3 (On "id" "f_id"))
               table2 (On "id" "f_id")
             )

query1 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String, String])))
query1 = tableQuery
           doubleJoin
           [("field1", Double), ("field2", Maybe String), ("field3", String)]

main : IO ()
main = do
  putStrLn "double-join select statement: "
  putStrLn $ select doubleJoin [("field1", Double), ("field2", Maybe String), ("field3", String)]

