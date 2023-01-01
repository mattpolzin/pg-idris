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
  , ("field3", col NonNullable PString)
  ]

query1 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query1 = tableQuery
           (leftJoin table1 table2 (on "table1.id" "table2.f_id"))
           [("table1.field1", Double), ("table2.field2", Maybe String)]

query2 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query2 = tableQuery
           (leftJoin table1 table2 (on "table1.id" "table2.f_id"))
           [("table1.field1", Double), ("table2.field3", Maybe String)]
--                                 ^ was NomNullable in original table, but nullable now because of left join

failing
  query3 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query3 = tableQuery
             (leftJoin table1 table2 (on "table1.id" "table2.problem"))
             --                                               ^
             -- Can't find "problem" in column names for table2
             [("table1.field1", Double), ("table2.field2", Maybe String)]

failing
  query4 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query4 = tableQuery
             (leftJoin table1 table2 (on "table1.id" "table2.f_id"))
             [("table1.field1", Double), ("table2.problem", Maybe String)]
             --                                    ^
             -- Can't find "problem" in (innerJoin table1 table2 (on "id" "f_id"))

failing
  query5 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query5 = tableQuery
             (leftJoin table1 table2 (on "table1.id" "table2.f_id"))
             [("table1.field1", Double), ("table2.field2", String)]
             --                                    ^
             -- Can't cast nullable Postgres string to Idris String (should be Maybe String)

failing
  query6 : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
  query6 = tableQuery
             (leftJoin table1 table2 (on "table1.id" "table2.f_id"))
             [("table1.field1", Double), ("table2.field3", String)]
             --                               ^
             -- Can't cast nullable Postgres string to Idris String (should be Maybe String)

query6' : Connection -> IO (Either String (rowCount ** Vect rowCount ?))
query6' = tableQuery
            (innerJoin table1 table2 (on "table1.id" "table2.f_id"))
            [("table1.field1", Double), ("table2.field3", String)]
            --                                ^
            -- Failed to select non-nullable String with leftJoin for query6 but succeeds with innerJoin
            -- for query6'

IdrCast PDouble Integer where
  toIdris _ = Just 1

query7 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Integer, Maybe String])))
query7 = tableQuery
           (leftJoin table1 table2 (on "table1.id" "table2.f_id"))
           [("table1.field1", Integer), ("table2.field2", Maybe String)]

