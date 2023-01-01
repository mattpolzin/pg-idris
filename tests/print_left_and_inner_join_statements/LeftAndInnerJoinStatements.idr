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
doubleJoin = (leftJoin
               (innerJoin table1 table2 (On "table1.id" "table2.f_id"))
               table3 (On "table1.id" "table3.f_id")
             )

query1 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String, Maybe String])))
query1 = tableQuery
           doubleJoin
           [("table1.field1", Double), ("table2.field2", Maybe String), ("table3.field3", Maybe String)]

main : IO ()
main = do
  putStrLn "inner-join followed by left-join select statement: "
  putStrLn $ select doubleJoin [("table1.field1", Double), ("table2.field2", Maybe String), ("table3.field3", Maybe String)]

