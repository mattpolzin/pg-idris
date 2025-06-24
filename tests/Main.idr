module Main

import TestLib
import Test.Golden
import System
import Postgres

compileTimeTests : Lazy (IO TestPool)
compileTimeTests = testsInDir "compile_time" "compile-time tests"

unitTests : Lazy (IO TestPool)
unitTests = testsInDir "unit_tests" "unit tests"

integrationTests : Lazy (IO TestPool)
integrationTests = testsInDir "integration" "integration tests with postgres"

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
  runner [ !compileTimeTests
         , !unitTests
         , !integrationTests
         ]

