module Postgres.Notification

import System.FFI
import Postgres.FFI.Utility
import Postgres.Data.Conn
import Postgres.Data.ResultStatus
import Postgres.DB.Core
import Postgres.DB.Wait
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
isNullNotification ptr = intToBool $ prim__isNullNotifyStruct ptr 

--
-- Listen
--

||| Start listening for notifications on the given channel.
export
pgListen : (channel: String) -> Conn -> IO ResultStatus
pgListen channel conn = withExecResult conn ("LISTEN " ++ channel) (\r => pure $ pgResultStatus r)

--
-- Retrieve
--

||| Gets the next notification _of those sitting around locally_.
||| Returns `Nothing` if there are no notifications.
|||
||| See `libpq` documentation on `PQnotifies` for details on the
||| distinction between retrieving notifications from the server and
||| getting the next notification that has already been retrieved.
|||
||| NOTE: This function _does_ consume input to make sure no notification
|||  sent by the server but not processed by the client yet gets
|||  missed.
export
pgNextNotification : Conn -> IO (Maybe Notification)
pgNextNotification (MkConn conn) = do True <- pgConsumeInput (MkConn conn)
                                        | False => pure Nothing
                                      notify <- primIO $ prim__dbGetNextNotification conn
                                      if isNullNotification notify
                                         then pure $ Nothing
                                         else do derefNotify <- notificationStruct notify
                                                 pure $ Just (notification derefNotify)

--
-- Loop Retrieval
--

||| First waits for activity from server
||| then checks if there is a new notification
||| then either returns a notification or cycles
cycle : Conn -> IO Notification
cycle conn = do True <- pgWait conn
                  | False => cycle conn
                Nothing <- pgNextNotification conn
                 | Just n => pure n
                cycle conn

||| Produce a potentially infinite stream of notifications.
||| Unlike `pgNextNotificaiton`, this will wait for the server
||| to deliver a notification.
|||
||| This _is_ an infinite sequence with a blocking wait for the
||| next notification so it is of somewhat limited utility
||| compared to checking for new notifications at a natural point
||| in your programs existing logic loop unless your entire loop
||| is dictated by notification arrival anyway.
export
partial
pgNotificationStream : Conn -> Stream (IO Notification)
pgNotificationStream conn = next :: (pgNotificationStream conn) where
  next : IO Notification
  next = do Nothing <- pgNextNotification conn
             | Just n => pure n
            cycle conn 

