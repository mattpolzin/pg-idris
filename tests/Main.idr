module Main

import TestLib
import Test.Golden
import Postgres

tests : TestPool
tests = MkTestPool [] [
  "expected_type_query"
]

main : IO ()
main = do
  Right config <- getTestConfig
    | Left err => putStrLn err
  True <- withTestDB {setup=True} $ do
            liftIO' . putStrLn $ "Testing against " ++ config.databaseUrl
            dbSetup
    | False => putStrLn "Cannot run tests without test database."
  runner [tests]

