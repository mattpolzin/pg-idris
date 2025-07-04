import Postgres
import Postgres.Data.PostgresTable
import TestLib

setupQuery1 : String
setupQuery1 = "drop table if exists public.table"

setupQuery2 : String
setupQuery2 = "create table public.table (i integer not null unique, d float8 not null, b bool not null, t text not null, c char not null, j json not null, ai integer[] not null, dm float8, astr text[])"

table1 : PersistedTable
table1 = PT "table" [
    ("i", col NonNullable PInteger)
  , ("d", col NonNullable PDouble)
  , ("b", col NonNullable PBoolean)
  , ("t", col NonNullable PString)
  , ("c", col NonNullable PChar)
  , ("j", col NonNullable PJson)
  , ("ai", col NonNullable (PArray PInteger))
  , ("dm", col Nullable PDouble)
  , ("astr", col NonNullable (PArray PString))
  ]

main : IO ()
main = 
  ignore . withTestDB $ TransitionIndexed.do
    res1 <- exec $ perform setupQuery1
    liftIO' . putStrLn $ show res1
    res2 <- exec $ perform setupQuery2
    liftIO' . putStrLn $ show res2
    -- first explicitly referring to table being inserted into (e.g. "table.i" for column "i"):
    COMMAND_OK <- exec $ tableInsert table1 ["table.i", "table.d", "table.b", "table.t", "table.c", "table.j", "table.ai", "table.dm", "table.astr"]
                                            [1, 2.5, True, "hello", 'c', JString "world", the (List Nat) [1,2,3], the (Maybe Double) Nothing, the (List String) ["hi", "how", "are", "you"]]
      | err => liftIO' $ printLn err
    -- then referring only to the column being inserted into (e.g. "i" for column "i"):
    COMMAND_OK <- exec $ tableInsert table1 ["i", "d", "b", "t", "c", "j", "ai", "dm", "astr"]
                                            [2, 2.5, True, "hello", 'c', JString "world", the (List Nat) [1,2,3], the (Maybe Double) Nothing, the (List String) ["hi", "how", "are", "you"]]
      | err => liftIO' $ printLn err
    liftIO' $ putStrLn "DONE"

