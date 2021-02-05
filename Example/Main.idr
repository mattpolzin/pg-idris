module Main

import Postgres
import Data.Strings
import Data.List
import Data.HVect

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

execStatement : Connection -> IO ()
execStatement conn = do
  cmd <- getLine
  Just json <- jsonQuery cmd conn 
   | Nothing => putErr "command failed."
  putStrLn $ show json

run : Connection -> IO ()
run conn = do
    putStrLn "Connected"

    COMMAND_OK <- listen "test_channel" conn
     | x => putErr {ctx="Listening"} x

    putStrLn "Choose One:"
    putStrLn "1 => Arbitrary Statement"
    putStrLn "2 => Notification Loop"
    opt <- getLine
    case (stringToNatOrZ opt) of
         1 => do putStrLn "Enter SQL: "
                 execStatement conn
         2 => do putStrLn "Entering notification loop.."
                 notificationLoop $ notificationStream conn
         _ => pure ()

public export
main : IO ()
main = do
  putStrLn "postgres url:"
  url <- getLine
  putStrLn "starting up..."

  Right _ <- withDB url $ do debugDumpTypes
                             Right (_ ** _ ** (headers, results)) <- exec $ stringQuery True "select * from pg_type limit 10"
                               | Left err => liftIO' $ putStrLn err
                               | _ => liftIO' $ putStrLn "ERROR"
                             Right (_ ** rows) <- exec $ expectedQuery [String, String, Bool, Bool] "select schemaname, tablename, hasindexes, hastriggers from pg_catalog.pg_tables limit 10"
                               | Left err => liftIO' $ putStrLn err
                             liftIO' $ putStrLn $ show rows
                             liftIO' $ putStrLn (unlines $ toList $ map (\(MkHeader n t) => n ++ ": " ++ (show t)) headers)
                             exec run
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

