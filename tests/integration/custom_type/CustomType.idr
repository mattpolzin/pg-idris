import Postgres
import Postgres.Data.PostgresType
import Postgres.Data.PostgresValue
import Postgres.Data.PostgresTable
import TestLib

setupQuery0 : String
setupQuery0 = "drop type if exists public.my_type"

setupQuery1 : String
setupQuery1 = "create type public.my_type as enum ('one', 'two', 'three')"

setupQuery2 : String
setupQuery2 = "drop table if exists public.table"

setupQuery3 : String
setupQuery3 = "create table public.table (id INT PRIMARY KEY, thingy MY_TYPE)"

setupQuery4 : String
setupQuery4 = "insert into public.table values (1, 'one'), (2, 'two'), (3, 'three')"

table1 : PersistedTable
table1 = pgTable "table" [
    ("id"    , NonNullable, PInteger)
  , ("thingy", NonNullable, POther "MY_TYPE")
  ]

IdrCast (POther "MY_TYPE") String where
  toIdris = Just . rawString

testQuery : Vect ? (String, Type)
testQuery = [
    ("id"    , Integer)
  , ("thingy", String)
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
    res3 <- exec $ perform setupQuery3
    liftIO' . putStrLn $ show res3
    res4 <- exec $ perform setupQuery4
    liftIO' . putStrLn $ show res4
    Right (rows ** res5) <- exec $ tableQuery' table1 testQuery
      | Left err => liftIO' $ putStrLn err
    liftIO' . for_ res5 $ putStrLn . show
