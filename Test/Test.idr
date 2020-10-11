module Test 

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

loop : Stream (IO Notification) -> IO ()
loop (n :: ns) = do putNotif n
                    loop ns

execStatement : Conn -> IO ()
execStatement db = do
  cmd <- getLine
  Just json <- pgJSONResult db cmd
   | Nothing => putErr "command failed."
  putStrLn $ show json

run : Conn -> IO ()
run db = do
    putStrLn "Connected"

    COMMAND_OK <- pgListen db "test_channel"
     | x => putErr {ctx="Listening"} x

    putStrLn "Choose One:"
    putStrLn "1 => Arbitrary Statement"
    putStrLn "2 => Notification Loop"
    opt <- getLine
    case (stringToNatOrZ opt) of
         1 => do putStrLn "Enter SQL: "
                 execStatement db
         2 => do putStrLn "Entering notification loop.."
                 loop $ pgNotificationStream db
         _ => pure ()

public export
main : IO ()
main = do
  putStrLn "postgres url:"
  url <- getLine
  putStrLn "starting up..."

  withDB url run $ putErr {ctx="Connection"}

  putStrLn "shutting down..."
