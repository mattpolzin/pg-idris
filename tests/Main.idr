module Main

import TestLib
import Test.Golden
import System
import Postgres

compileTimeTests : TestPool
compileTimeTests = MkTestPool "compile-time" [] Nothing [
  "join_statement"
]

integrationTests : TestPool
integrationTests = MkTestPool "postgres" [] Nothing [
  -- database query tests
  "expected_type_query"
, "table_query"
, "join_query"
, "insert_query"
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
  runner [compileTimeTests, integrationTests]

