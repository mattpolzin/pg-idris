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

query1 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query1 = tableQuery
           (innerJoin table1 table2 (On "table1.id" "table2.f_id"))
           [("table1.field1", Double), ("table2.field2", Maybe String)]

-- now with (==) `on` function:
query2 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query2 = tableQuery
           (innerJoin table1 table2 ("table1.id" == "table2.f_id"))
           [("table1.field1", Double), ("table2.field2", Maybe String)]

-- now with (==) `on` and no table references function:
query2' : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query2' = tableQuery
            (innerJoin table1 table2 ("id" == "f_id"))
            [("table1.field1", Double), ("table2.field2", Maybe String)]

-- now with lower-case `on` function:
query2'' : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query2'' = tableQuery
             (innerJoin table1 table2 (on "table1.id" "table2.f_id"))
             [("table1.field1", Double), ("table2.field2", Maybe String)]

-- and finally without referencing the tables explicitly with lower-case `on` function:
query2''' : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query2''' = tableQuery
              (innerJoin table1 table2 (on "id" "f_id"))
              [("table1.field1", Double), ("table2.field2", Maybe String)]

failing
  query3 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query3 = tableQuery
             (innerJoin table1 table2 (on "table1.id" "table2.problem"))
             --                                   ^
             -- Can't find "problem" in column names for table2
             [("table1.field1", Double), ("table2.field2", Maybe String)]

failing
  query4 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query4 = tableQuery
             (innerJoin table1 table2 (on "table1.id" "table2.f_id"))
             [("table1.field1", Double), ("table2.problem", Maybe String)]
             --                       ^
             -- Can't find "problem" in (innerJoin table1 table2 (on "id" "f_id"))

failing
  query5 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query5 = tableQuery
             (innerJoin table1 table2 (on "table1.id" "table2.f_id"))
             [("table1.field1", Double), ("table2.field2", String)]
             --                               ^
             -- Can't cast nullable Postgres string to Idris String (should be Maybe String)

IdrCast PDouble Integer where
  toIdris _ = Just 1

query6 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Integer, Maybe String])))
query6 = tableQuery
           (innerJoin table1 table2 (on "table1.id" "table2.f_id"))
           [("table1.field1", Integer), ("table2.field2", Maybe String)]

