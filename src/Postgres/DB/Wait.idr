module Postgres.DB.Wait

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.DB.Core

%foreign helper "socket_wait"
prim__dbWait : Ptr PGconn -> PrimIO Int

||| Wait for something to happen on the given
||| connection.
|||
||| NOTE: Will call to consume as a convenience
|||  after finished waiting.
|||
||| Returns True if waiting succeeds and False
||| if waiting fails for some reason.
export
pgWait : Conn -> IO Bool
pgWait (MkConn conn) = do True <- (map boolValue (primIO $ prim__dbWait conn)) 
                           | False => pure False
                          pgConsumeInput (MkConn conn)
