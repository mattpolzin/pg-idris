module Postgres.Exec

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.DB
import Postgres.Result

%foreign libpq "PQexec"
prim__dbExec : Ptr PGconn -> (command: String) -> PrimIO (Ptr PGresult)

pgExec : HasIO io => Conn -> (command: String) -> io (Ptr PGresult)
pgExec (MkConn conn) command = primIO $ prim__dbExec conn command

export
data ExecSource : Type where
  MkExecSource : Conn -> ExecSource

public export
ResultProducer ExecSource String where
  exec (MkExecSource conn) = pgExec conn 

export
pgSafeExec : Conn -> (command : String) -> (f : Result -> b) -> IO b
pgSafeExec conn command = Executor (MkExecSource conn) command

