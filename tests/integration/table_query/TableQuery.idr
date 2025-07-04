import Postgres
import Postgres.Data.PostgresTable
import TestLib

setupQuery0 : String
setupQuery0 = "drop table if exists public.table"

setupQuery1 : String
setupQuery1 = "create table public.table (i integer not null, d float8 not null, b bool not null, t text not null, c char not null, j json not null, ai integer[] not null, dm float8)"

setupQuery2 : String
setupQuery2 = "insert into public.table (i, d, b, t, c, j, ai, dm) values (1, 2.5, true, 'hello', 'c', '{\"hello\": \"world\"}', '{1,2,3}', null)"

table1 : PersistedTable
table1 = pgTable "table" [
    ("i", NonNullable, PInteger)
  , ("d", NonNullable, PDouble)
  , ("b", NonNullable, PBoolean)
  , ("t", NonNullable, PString)
  , ("c", NonNullable, PChar)
  , ("j", NonNullable, PJson)
  , ("ai", NonNullable, (PArray PInteger))
  , ("dm", Nullable, PDouble)
  ]

testQuery : Vect ? (String, Type)
testQuery = [
    ("i", Integer)
  , ("d", Double)
  , ("b", Bool)
  , ("t", String)
  , ("c", Char)
  , ("j", JSON)
  , ("ai", (List Integer))
  , ("dm", Maybe Double)
  ]

main : IO ()
main = 
  ignore . withTestDB $ TransitionIndexed.do
    res0 <- exec $ perform setupQuery0
    liftIO' . putStrLn $ show res0
    res1 <- exec $ perform setupQuery1
    liftIO' . putStrLn $ show res1
    res2 <- exec $ perform setupQuery2
    liftIO' . putStrLn $ show res2
    Right (rows ** res3) <- exec $ tableQuery' table1 testQuery
      | Left err => liftIO' $ putStrLn err
    liftIO' . for_ res3 $ putStrLn . show
