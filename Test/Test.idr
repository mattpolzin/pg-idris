module Test 

import Postgres
import Postgres.Data
import Postgres.DB.Core
import Data.Strings

error : Show a => {default "" ctx: String} -> a -> String
error {ctx} diag = let ctxStr = if (strLength ctx) == 0
                                   then ""
                                   else " (" ++ ctx ++ ")" in 
                                     "Error" ++ ctxStr ++ ": " ++ (show diag)

putErr : Show a => {default "" ctx: String} -> a -> IO ()
putErr {ctx} diag = putStrLn $ error {ctx=ctx} diag

run : Conn -> IO ()
run db = do
    putStrLn "Connected"

    COMMAND_OK <- pgListen db "test_channel"
     | x => putErr {ctx="Listening"} x

    putStrLn "Waiting for notifications..."

    True <- pgWait db
     | False => putErr {ctx="Waiting"} "Somehow failed while waiting for notifications."

    Just n <- pgGetNextNotification db
     | Nothing => putStrLn "No notifications"

    putStrLn $ "notification on channel " ++ n.channel

public export
main : IO ()
main = do
  putStrLn "postgres url:"
  url <- getLine
  putStrLn "starting up..."

  withDB url run (\err => putErr {ctx="Connection"} err)

  putStrLn "shutting down..."
