module Postgres.DB

import Postgres.DB.Core
import Postgres.Data.Conn
import Postgres.Data.ConnectionStatus

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

export
data DBState = Open | Closed

--
-- Database abstraction
--

export
data Database : Type -> DBState -> DBState -> Type where
  DBOpen  : (url : String) -> Database () Closed Open
  DBClose : Database () Open Closed

  Exec    : (fn : Conn -> IO a) -> Database a Open Open

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

-- TODO: handle failure to connect as is done in `withConn` function.

-- idea: create ExecutionContext (monad?) to encapsulate the below storage of a connection alongside
-- the database in various states.

mutual
  runToOpen : HasIO io => Database a Closed Open -> io $ (Conn, a)
  runToOpen (DBOpen url) = pure (!(pgOpen url), ())
  runToOpen (db `Bind` f) with (runningDatabase db)
    runToOpen (db `Bind` f) | (Connected db) = do (conn, res) <- runToOpen db
                                                  runOpen conn (f res)
    runToOpen (db `Bind` f) | (Disconnected db) = do res <- runDatabase db
                                                     runToOpen (f res)

  runToClose : HasIO io => (conn : Conn) -> Database a Open Closed -> io a
  runToClose conn DBClose = pgClose conn
  runToClose conn (db `Bind` f) with (runningDatabase db)
    runToClose conn (db `Bind` f) | (Connected db) = do (conn', res) <- runOpen conn db
                                                        runToClose conn' (f res)
    runToClose conn (db `Bind` f) | (Disconnected db) = do res <- runToClose conn db
                                                           runDatabase (f res)

  runOpen : HasIO io => (conn : Conn) -> Database a Open Open -> io $ (Conn, a)
  runOpen conn (Exec fn) = liftIO [ (conn, res) | res <- fn conn ]
  runOpen conn (db `Bind` f) with (runningDatabase db)
    runOpen conn (db `Bind` f) | (Connected db) = do (conn', res) <- runOpen conn db
                                                     runOpen conn' (f res)
    runOpen conn (db `Bind` f) | (Disconnected db) = do res <- runToClose conn db
                                                        runToOpen (f res)

  export
  runDatabase : HasIO io => Database a Closed Closed -> io a
  runDatabase (Pure x) = pure x
  runDatabase (db `Bind` f) with (runningDatabase db)
    runDatabase (db `Bind` f) | (Connected db) = do (conn, res) <- runToOpen db
                                                    runToClose conn (f res)
    runDatabase (db `Bind` f) | (Disconnected db) = do res <- runDatabase db
                                                       runDatabase (f res)

export
openDatabase : (url : String) -> Database () Closed Open
openDatabase url = DBOpen url

export
closeDatabase : Database () Open Closed
closeDatabase = DBClose

export
exec : (Conn -> IO a) -> Database a Open Open
exec f = Exec f


