module TestLib

import Postgres
import System
import Data.String
import Data.String.Extra
import Data.List
import Data.List1

showHVectCSV : All Show cs => HVect cs -> String
showHVectCSV @{[]} [] = ""
showHVectCSV @{(z :: [])} (x :: []) = show x
showHVectCSV @{(z :: (w :: s))} (x :: (y :: v)) = "\{show x}, \{showHVectCSV (y :: v)}"

export
All Show cs => Show (HVect cs) where
  show vs = "[\{showHVectCSV vs}]"

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
      join "/" $ (allButDatabase `snoc` "pg_idris_test")

public export
record Config where
  constructor MkConfig
  databaseUrl : String

export
getTestConfig : HasIO io => io (Either String Config)
getTestConfig = do 
  Just url <- databaseUrl
    | Nothing => pure $ Left "Missing TEST_DATABASE_URL environment variable needed for testing."
  pure $ Right $ MkConfig url

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
withTestDB : HasIO io => {default False setup : Bool} -> (run : Database () Open (const Open)) -> io Bool
withTestDB {setup} run = do
  Right config <- getTestConfig
    | Left err => do putStrLn err
                     pure False
  let databaseUrl : String = if setup then config.databaseUrl else testDatabaseUrl config.databaseUrl
  Right () <- withDB databaseUrl run
    | Left err => do putStrLn err
                     pure False
  pure True

