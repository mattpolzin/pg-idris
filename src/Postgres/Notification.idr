module Postgres.Notification

import System.FFI
import Postgres.Utility
import Postgres.Data.Conn
import Postgres.DB
import Postgres.Result
import Postgres.Exec

||| Internal type representative of the libpq struct of the same
||| name.
PGnotify : Type
PGnotify = Struct "PGnotify" [("relname", (Ptr String)), ("be_pid", Int), ("extra", (Ptr String))]

public export
record Notification where
  constructor MkNotification
  channel : String
  payload : String

notificationChannel : PGnotify -> String
notificationChannel n = prim__string_value $ getField n "relname"

notificationPayload : PGnotify -> String
notificationPayload n = prim__string_value $ getField n "extra"

notification : PGnotify -> Notification
notification n = MkNotification (notificationChannel n) (notificationPayload n)

%foreign libpq "PQnotifies"
prim__dbGetNextNotification : Ptr PGconn -> PrimIO (Ptr PGnotify)

||| Takes a PGnotify struct pointer to a PGnotify struct.
||| IMPORTANT: Be sure you have checked that the struct pointer
|||    is non-null before calling this.
|||
||| NOTE: Unless you need to hold onto the PGnotify struct for longer,
|||    call the notificationStruct function instead which will take care
|||    of freeing memory for you.
%foreign helper "notify_struct"
prim__dbNotifyStruct : Ptr PGnotify -> PGnotify

%foreign libpq "PQfreemem"
prim__dbFreeNotifyStruct : Ptr PGnotify -> PrimIO ()

||| Takes a PGnotify struct pointer to a PGnotify struct.
||| IMPORTANT: Be sure you have checked that the struct pointer
|||    is non-null before calling this.
notificationStruct : HasIO io => Ptr PGnotify -> io PGnotify
notificationStruct ptr = let res = prim__dbNotifyStruct ptr in
                             do primIO $ prim__dbFreeNotifyStruct ptr
                                pure res

%foreign helper "is_null"
prim__isNullNotifyStruct : Ptr PGnotify -> Int

isNullNotification : Ptr PGnotify -> Bool
isNullNotification ptr = boolValue $ prim__isNullNotifyStruct ptr where
  boolValue : Int -> Bool
  boolValue 0 = False
  boolValue _ = True

--
-- Retrieve
--

export
pgGetNextNotification : Conn -> IO (Maybe Notification)
pgGetNextNotification (MkConn conn) = do notify <- primIO $ prim__dbGetNextNotification conn
                                         if isNullNotification notify
                                            then pure $ Nothing
                                            else do derefNotify <- notificationStruct notify
                                                    pure $ Just (notification derefNotify)

--
-- Listen
--

export
pgListen : Conn -> (channel: String) -> IO ResultStatus
pgListen conn channel = pgSafeExec conn ("LISTEN " ++ channel) pgResultStatus

