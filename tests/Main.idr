module Main

import TestLib
import Test.Golden
import System
import Postgres

compileTimeTests : TestPool
compileTimeTests = MkTestPool "compile-time tests" [] Nothing [
  "inner_join_statement"
, "left_join_statement"
, "alias_statement"
]

unitTests : TestPool
unitTests = MkTestPool "unit tests" [] Nothing [
  "print_double_join_statement"
, "print_left_and_inner_join_statements"
, "print_alias_statement"
]

integrationTests : TestPool
integrationTests = MkTestPool "integration tests with postgres" [] Nothing [
  -- database query tests
  "expected_type_query"
, "table_query"
, "inner_join_query"
, "left_join_query"
, "insert_query"
, "alias_query"
, "json_query"
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
  runner [compileTimeTests, unitTests, integrationTests]

