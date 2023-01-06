module Main

import Postgres
import Postgres.Data.PostgresTable
import Data.String
import Data.String.Extra
import Data.List

error : Show a => {default "" ctx: String} -> a -> String
error {ctx} diag = let ctxStr = if (strLength ctx) == 0
                                   then ""
                                   else " (" ++ ctx ++ ")" in 
                                     "Error" ++ ctxStr ++ ": " ++ (show diag)

putErr : Show a => {default "" ctx: String} -> a -> IO ()
putErr {ctx} diag = putStrLn $ error {ctx=ctx} diag

putNotif : IO Notification -> IO ()
putNotif n = putStrLn $ "Notification on channel " ++ (show (!n).channel)

notificationLoop : Stream (IO Notification) -> IO ()
notificationLoop (n :: ns) = do putNotif n
                                notificationLoop ns

jsonStatement : Connection -> IO ()
jsonStatement conn = do
  cmd <- getLine
  Just json <- jsonQuery cmd conn 
   | Nothing => putErr "command failed."
  putStrLn $ show json

describe : String -> Connection -> IO ()
describe query conn = do
  Right (_ ** _ ** (headers, _)) <- stringQuery True query conn
    | Left err => putStrLn err
  putStrLn (join ", " $ show <$> headers)

describeResult : Connection -> IO ()
describeResult conn = do
  query <- getLine
  describe query conn
  
showResult : Connection -> IO ()
showResult conn = do
  query <- getLine
  Right (_ ** _ ** rows) <- stringQuery False query conn
    | Left err => putStrLn err
  for_ rows $ \row => do
    putStrLn (join ", " $ (\case Just s => s; Nothing => "null") <$> row)

showTables : Connection -> IO ()
showTables conn = do
  Right (_ ** rows) <- expectedQuery [String, String, Bool] "select schemaname, tablename, hasindexes from pg_tables limit 10" conn
    | Left err => putStrLn err
  for_ rows $ \(schema :: table :: hasIndices :: []) => do
    putStrLn $ schema ++ "." ++ table ++ (if hasIndices then " (has indices)" else " (doesn't have indices)")

functionsTable : PersistedTable
functionsTable = PT "pg_proc" [ ("proname", col NonNullable PString)
                              , ("proargnames", col Nullable (PArray PString))
                              ]

showFunctions : Connection -> IO ()
showFunctions conn = do
  Right (_ ** rows) <- tableQuery functionsTable [("proname", String), ("proargnames", Maybe (List String))] conn
    | Left err => putStrLn err
  for_ rows $ \(name :: args :: []) => do
    putStrLn $ name ++ "(" ++ (join ", " (maybe [] id args)) ++ ")"

describeFnTable : Connection -> IO ()
describeFnTable conn = do
  describe "select * from pg_proc limit 1" conn

run : Connection -> IO ()
run conn = Prelude.do
  putStrLn "Connected"

  COMMAND_OK <- listen "test_channel" conn
   | x => putErr {ctx="Listening"} x

  putStrLn "Choose One:"
  putStrLn "1 => Arbitrary JSON Statement"
  putStrLn "2 => Notification Loop"
  putStrLn "3 => Describe Arbitrary Result"
  putStrLn "4 => Describe pg_proc table."
  putStrLn "5 => Show Arbitrary SELECT Result Rows"
  putStrLn "6 => Show 10 tables."
  putStrLn "7 => Show All functions."
  opt <- getLine
  case (stringToNatOrZ opt) of
       1 => do putStrLn "Enter SQL: "
               jsonStatement conn
       2 => do putStrLn "Entering notification loop..."
               notificationLoop $ notificationStream conn
       3 => do putStrLn "Enter SQL: "
               describeResult conn
       4 => describeFnTable conn
       5 => do putStrLn "Enter SQL: "
               showResult conn
       6 => do putStrLn "Showing 10 tables..."
               showTables conn
       7 => do putStrLn "Showing 10 functions..."
               showFunctions conn
       _ => pure ()

public export
main : IO ()
main = Prelude.do
  putStrLn "postgres url:"
  url <- getLine
  putStrLn "starting up..."

  Right _ <- withDB url (exec run)
    | Left err => putErr {ctx="Connection"} err

  putStrLn "shutting down..."



--
-- As seen in README
--

-- Just an example of opening and closing; it's easier to use
-- `withDB`.
openAndClose : (url : String) -> Database () Closed (const Closed)
openAndClose url =
  do initDatabase
     OK <- openDatabase url
       | Failed err => pure () -- connection error!
     closeDatabase

-- Again, an example used in the Readme
runRoutine : HasIO io => (url : String) -> io (Either String ())
runRoutine url =
  withDB url $ pure ()

-- more example
execCommand : Database () Open (const Open)
execCommand = do liftIO' $ putStrLn "Woo! Running a command!"
                 exec $ (\c => pure ())

