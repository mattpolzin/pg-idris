import Postgres
import Postgres.Data.PostgresTable
import TestLib

setupQuery00 : String
setupQuery00 = "drop table if exists public.table2"

setupQuery01 : String
setupQuery01 = "drop table if exists public.table1"

setupQuery10 : String
setupQuery10 = "create table public.table1 (i integer not null unique, d float8 not null, b bool not null, t text not null, c char not null, j json not null, ai integer[] not null, dm float8)"

setupQuery11 : String
setupQuery11 = "create table public.table2 (f_i integer not null references table1 (i), extra1 text not null, extra2 text not null)"

setupQuery20 : String
setupQuery20 = "insert into public.table1 (i, d, b, t, c, j, ai, dm) values (1, 2.5, true, 'hello', 'c', '{\"hello\": \"world\"}', '{1,2,3}', null)"

setupQuery21 : String
setupQuery21 = "insert into public.table2 (f_i, extra1, extra2) values (1, 'hello', 'world')"

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

-- table 2 is a runtime table for no particular reason, just testing joining persisted & runtime together.
table2 : RuntimeTable
table2 = RT (named "table2") [
    ("table2.f_i"   , col NonNullable PInteger)
  , ("table2.extra1", col NonNullable PString)
  , ("table2.extra2", col NonNullable PString)
  ]

testQuery : Vect ? (ColumnIdentifier, Type)
testQuery = [
    ("table1.i", Integer)
  , ("table1.d", Double)
  , ("table1.b", Bool)
  , ("table1.t", String)
  , ("table1.c", Char)
  , ("table1.j", JSON)
  , ("table1.ai", (List Integer))
  , ("table1.dm", Maybe Double)
  , ("table2.extra1", String)
  , ("table2.extra2", String)
  ]

main : IO ()
main = 
  ignore . withTestDB $ TransitionIndexed.do
    res00 <- exec $ perform setupQuery00
    liftIO' . putStrLn $ show res00
    res01 <- exec $ perform setupQuery01
    liftIO' . putStrLn $ show res01
    res10 <- exec $ perform setupQuery10
    liftIO' . putStrLn $ show res10
    res11 <- exec $ perform setupQuery11
    liftIO' . putStrLn $ show res11
    res20 <- exec $ perform setupQuery20
    liftIO' . putStrLn $ show res20
    res21 <- exec $ perform setupQuery21
    liftIO' . putStrLn $ show res21
    Right (rows ** res3) <- exec $ tableQuery (innerJoin table1 table2 (On "table1.i" "table2.f_i")) testQuery
      | Left err => liftIO' $ putStrLn err
    liftIO' . for_ res3 $ putStrLn . show

