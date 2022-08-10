module Postgres.Exec

import Postgres.FFI.Utility
import Postgres.Data.Conn
import Postgres.Result

%foreign libpq "PQexec"
prim__dbExec : Ptr PGconn -> (command: String) -> PrimIO (Ptr PGresult)

data ExecSource : Type where
  MkExecSource : Conn -> ExecSource

ResultProducer ExecSource String where
  exec (MkExecSource conn) = pgExec conn where
     pgExec : Conn -> (command: String) -> IO (Ptr PGresult)
     pgExec (MkConn conn) command = primIO $ prim__dbExec conn command

||| Execute the given command, apply the given function `f`,
||| and then free the memory for the result.
export
withExecResult : Conn -> (command : String) -> (f : Result -> IO b) -> IO b
withExecResult conn command = withResult (MkExecSource conn) command

export
ignore : Result -> IO ()
ignore _ = pure ()
