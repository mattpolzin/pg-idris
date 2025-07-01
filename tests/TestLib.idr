module TestLib

import Postgres
import System
import Data.String
import Data.List
import Data.List1

export
databaseUrl : HasIO io => io (Maybe String)
databaseUrl = getEnv "TEST_DATABASE_URL"

||| Strip the database name off the end of the database URL
||| and append the test database.
testDatabaseUrl : String -> String
testDatabaseUrl url = 
  let splitUrl = split (== '/') url
      allButDatabase = init splitUrl
  in
      joinBy "/" $ (allButDatabase `snoc` "pg_idris_test")

public export
record Config where
  constructor MkConfig
  databaseUrl : String

public export
data Mode = Full Config | CompTime | Err String

export
getTestConfig : HasIO io => io Mode
getTestConfig = do 
  Just url <- databaseUrl
    | Nothing => tryPartial
  pure $ Full $ MkConfig url

  where
    tryPartial : io Mode
    tryPartial = Prelude.do
      ("--only" :: "compile_time" :: _) <- drop 2 <$> getArgs
        | _ => pure $ Err "Missing TEST_DATABASE_URL environment variable needed for testing."
      pure CompTime

export
dbSetup : Database () Open (const Open)
dbSetup = TransitionIndexed.do
  liftIO' $ putStrLn "Setting database up"
  res1 <- exec $ perform "drop database if exists pg_idris_test"
  liftIO' . putStrLn $ show res1
  res2 <- exec $ perform "create database pg_idris_test"
  liftIO' . putStrLn $ show res2
  liftIO' $ putStrLn "test database created"

export
withTestDB : HasIO io => {default False setup : Bool} -> Config -> (run : Database () Open (const Open)) -> io Bool
withTestDB {setup} config run = do
  let databaseUrl : String = if setup then config.databaseUrl else testDatabaseUrl config.databaseUrl
  Right () <- withDB databaseUrl run
    | Left err => do putStrLn err
                     pure False
  pure True

