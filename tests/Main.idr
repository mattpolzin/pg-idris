module Main

import TestLib
import Test.Golden
import System
import Postgres

tests : TestPool
tests = MkTestPool "postgres" [] [
  "expected_type_query"
]

exitError : String -> IO ()
exitError err = do
  putStrLn err
  exitFailure

main : IO ()
main = do
  Right config <- getTestConfig
    | Left err => exitError err
  True <- withTestDB {setup=True} $ do
            liftIO' . putStrLn $ "Testing against " ++ config.databaseUrl
            dbSetup
    | False => exitError "Cannot run tests without test database."
  runner [tests]

