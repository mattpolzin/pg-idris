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

query1 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query1 = tableQuery
           (innerJoin table1 table2 (On "id" "f_id"))
           [("field1", Double), ("field2", Maybe String)]

failing
  query2 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query2 = tableQuery
             (innerJoin table1 table2 (On "id" "problem"))
             --                                   ^
             -- Can't find "problem" in column names for table2
             [("field1", Double), ("field2", Maybe String)]

failing
  query3 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query3 = tableQuery
             (innerJoin table1 table2 (On "id" "f_id"))
             [("field1", Double), ("problem", Maybe String)]
             --                       ^
             -- Can't find "problem" in (innerJoin table1 table2 (On "id" "f_id"))

failing
  query4 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query4 = tableQuery
             (innerJoin table1 table2 (On "id" "f_id"))
             [("field1", Double), ("field2", String)]
             --                               ^
             -- Can't cast nullable Postgres string to Idris String (should be Maybe String)

IdrCast PDouble Integer where
  toIdris _ = Just 1

query5 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Integer, Maybe String])))
query5 = tableQuery
           (innerJoin table1 table2 (On "id" "f_id"))
           [("field1", Integer), ("field2", Maybe String)]

