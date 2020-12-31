module Postgres.DB

import Postgres.DB.Core
import Postgres.Data.Conn
import Postgres.Data.ConnectionStatus
import Postgres.Data.ResultStatus
import Postgres.Query
import Postgres.Notification
import Language.JSON

--
-- Safe connection
--

||| Open a connection to a Postgres database,
||| perform some actions with it, and then close
||| the connection.
|||
||| If the database connection is established successfully,
||| the `onOpen` function will be executed. If the
||| connection cannot be established, `onError` will be
||| executed.
|||
||| You do _not_ call `pgOpen` or `pgClose` directly
||| when using this function.
export
withConn : HasIO io => (pgUrl: String) -> (onOpen: Conn -> io b) -> (onError: ConnectionStatus -> io b) -> io b
withConn pgUrl onOpen onError 
  = do conn <- pgOpen pgUrl
       out <- case (pgStatus conn) of
                OK => onOpen conn
                x => onError x
       pgClose conn
       pure out

--
-- Database abstraction
--

export
data DBState = Open | Closed

export
data Connection : Type where
  MkConnection : Conn -> Connection

getConn : Connection -> Conn
getConn (MkConnection conn) = conn

export
data Database : Type -> DBState -> DBState -> Type where
  DBOpen  : (url : String) -> Database () Closed Open
  DBClose : Database () Open Closed
  OpenFailed : (error : String) -> Database () Closed Closed

  Exec    : (fn : Connection -> IO a) -> Database a Open Open

  Pure    : a -> Database a Closed Closed
  Bind    : {s1, s2 : DBState} -> (db : Database a s1 s2) -> (f : a -> Database b s2 s3) -> Database b s1 s3

%name Database db, db1, db2

export
(>>=) : {s1,s2 : DBState} -> (db : Database a s1 s2) -> (f : a -> Database b s2 s3) -> Database b s1 s3
(>>=) = Bind

export
initDatabase : Database () Closed Closed
initDatabase = Pure ()

data RunningDatabase : Database a s1 s2 -> Type where
  Connected : (db : Database a s1 Open) -> RunningDatabase db
  Disconnected : (db: Database a s1 Closed) -> RunningDatabase db

runningDatabase : {s2 : DBState} -> (db : Database a s1 s2) -> RunningDatabase db
runningDatabase {s2 = Open} db = Connected db
runningDatabase {s2 = Closed} db = Disconnected db

-- idea: create ExecutionContext (monad?) to encapsulate the below storage of a connection alongside
-- the database in various states.

mutual
  runToOpen : HasIO io => Database a Closed Open -> io $ Either String (Conn, a)
  runToOpen (DBOpen url) = do conn <- pgOpen url
                              let result = case (pgStatus conn) of
                                                OK => Right (conn, ())
                                                x => Left $ show x
                              pure result
  runToOpen (db `Bind` f) with (runningDatabase db)
    runToOpen (db `Bind` f) | (Connected db) = do Right (conn, res) <- runToOpen db
                                                    | Left err => pure $ Left err
                                                  runOpen conn (f res)
    runToOpen (db `Bind` f) | (Disconnected db) = do Right res <- runDatabase db
                                                       | Left err => pure $ Left err
                                                     runToOpen (f res)

  runToClose : HasIO io => (conn : Conn) -> Database a Open Closed -> io $ Either String a
  runToClose conn DBClose = (Right) <$> pgClose conn
  runToClose conn (db `Bind` f) with (runningDatabase db)
    runToClose conn (db `Bind` f) | (Connected db) = do Right (conn', res) <- runOpen conn db
                                                          | Left err => pure $ Left err
                                                        runToClose conn' (f res)
    runToClose conn (db `Bind` f) | (Disconnected db) = do Right res <- runToClose conn db
                                                             | Left err => pure $ Left err
                                                           runDatabase (f res)

  runOpen : HasIO io => (conn : Conn) -> Database a Open Open -> io $ Either String (Conn, a)
  runOpen conn (Exec fn) = liftIO [ Right (conn, res) | res <- fn $ MkConnection conn ]
  runOpen conn (db `Bind` f) with (runningDatabase db)
    runOpen conn (db `Bind` f) | (Connected db) = do Right (conn', res) <- runOpen conn db
                                                       | Left err => pure $ Left err
                                                     runOpen conn' (f res)
    runOpen conn (db `Bind` f) | (Disconnected db) = do Right res <- runToClose conn db
                                                          | Left err => pure $ Left err
                                                        runToOpen (f res)

  export
  runDatabase : HasIO io => Database a Closed Closed -> io $ Either String a
  runDatabase (Pure x) = pure $ Right x
  runDatabase (OpenFailed err) = pure $ Left err
  runDatabase (db `Bind` f) with (runningDatabase db)
    runDatabase (db `Bind` f) | (Connected db) = do Right (conn, res) <- runToOpen db
                                                      | Left err => pure $ Left err
                                                    runToClose conn (f res)
    runDatabase (db `Bind` f) | (Disconnected db) = do Right res <- runDatabase db
                                                         | Left err => pure $ Left err
                                                       runDatabase (f res)

export
openDatabase : (url : String) -> Database () Closed Open
openDatabase url = DBOpen url

export
closeDatabase : Database () Open Closed
closeDatabase = DBClose

export
exec : (Connection -> IO a) -> Database a Open Open
exec f = Exec f

||| Take a function that operates on a Conn
||| (currency of the underlying lower level
||| Postgres stuff) and turn it into a function
||| that operates on a Connection.
pgExec : (Conn -> IO a) -> Connection -> IO a
pgExec f = \c => f $ getConn c

export
withDB : HasIO io => (url : String) -> Database a Open Open -> io $ Either String a
withDB url dbOps = runDatabase $ do initDatabase
                                    openDatabase url
                                    out <- dbOps
                                    closeDatabase
                                    Pure out

--
-- Postgres Commands
--

||| Query the database expecting a JSON result is returned.
export 
jsonQuery : (query : String) -> Connection -> IO (Maybe JSON)
jsonQuery = pgExec . pgJSONResult

||| Start listening for notifications on the given channel.
export
listen : (channel : String) -> Connection -> IO ResultStatus
listen = pgExec . pgListen

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
nextNotification : Connection -> IO (Maybe Notification)
nextNotification = pgExec pgNextNotification

||| Produce a potentially infinite stream of notifications.
||| Unlike `nextNotificaiton`, this will wait for the server
||| to deliver a notification.
|||
||| This _is_ an infinite sequence with a blocking wait for the
||| next notification so it is of somewhat limited utility
||| compared to checking for new notifications at a natural point
||| in your programs existing logic loop unless your entire loop
||| is dictated by notification arrival anyway.
export
partial
notificationStream : Connection -> Stream (IO Notification)
notificationStream conn = pgNotificationStream (getConn conn)
