module Main

import Postgres
import Postgres.Data
import Postgres.DB.Core
import Postgres.Query
import Data.Strings
import Data.Nat
import Data.Fin
import Language.JSON

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

  Right _ <- withDB url $ exec run
    | Left err => putErr {ctx="Connection"} err
  -- withConn url run $ putErr {ctx="Connection"}

  putStrLn "shutting down..."
