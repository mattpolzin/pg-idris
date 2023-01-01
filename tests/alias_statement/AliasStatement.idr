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
           (innerJoin (table1 `as` "t") (table2 `as` "o") (on "t.id" "o.f_id"))
           [("t.field1", Double), ("o.field2", Maybe String)]

joinedTables : RuntimeTable
joinedTables = innerJoin table1 table2 (on "table1.id" "table2.f_id") `as` "hello"

query2 : Connection -> IO (Either String (rowCount ** Vect rowCount (HVect [Double, Maybe String])))
query2 = tableQuery (joinedTables `as` "new") [("new.field1", Double), ("new.field2", Maybe String)]

