import Postgres
import TestLib

setupQuery1 : String
setupQuery1 = "drop table if exists public.table"

setupQuery2 : String
setupQuery2 = "create table public.table (j json not null)"

setupQuery3 : String
setupQuery3 = """
              insert into public.table 
                values ('{ "hello": { "cool": true}, "top": "notch", "and": 2}')
              """

main : IO ()
main = 
  ignore . withTestDB $ TransitionIndexed.do
    res1 <- exec $ perform setupQuery1
    liftIO' . putStrLn $ show res1
    res2 <- exec $ perform setupQuery2
    liftIO' . putStrLn $ show res2
    res3 <- exec $ perform setupQuery3
    liftIO' . putStrLn $ show res3
    Just jsonRes <- exec $ jsonQuery "select j from public.table"
      | Nothing => liftIO' $ putStrLn "FAILED to get JSON result"
    liftIO' $ printLn jsonRes
    liftIO' $ putStrLn "DONE"

