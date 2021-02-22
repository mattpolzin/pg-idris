import Postgres
import TestLib

setupQuery1 : String
setupQuery1 = "create table public.expected_types (i integer, d float8, b bool, t text, c char, j json, ai integer[], dm float8)"

setupQuery2 : String
setupQuery2 = "insert into public.expected_types (i, d, b, t, c, j, ai, dm) values (1, 2.5, true, 'hello', 'c', '{\"hello\": \"world\"}', '{1,2,3}', null)"

testQuery : String
testQuery = "select * from public.expected_types"

main : IO ()
main = 
  ignore . withTestDB $ do
    res1 <- exec $ perform setupQuery1
    liftIO' . putStrLn $ show res1
    res2 <- exec $ perform setupQuery2
    liftIO' . putStrLn $ show res2
    Right (rows ** res3) <- exec $ expectedQuery [Integer, Double, Bool, String, Char, JSON, List Integer, Maybe Double] testQuery
      | Left err => liftIO' $ putStrLn err
    liftIO' . for_ res3 $ putStrLn . show
