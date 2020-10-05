module Postgres.DB.Core

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.Data.ConnectionStatus

--
-- Open Connection
--
%foreign (Utility.libpq "PQconnectdb")
prim__dbOpen : String -> PrimIO (Ptr PGconn)

||| Open a connection to a Postgres database.
||| This connection must be closed using `pgClose`.
|||
||| You can check the status of the connection
||| (including failure to connect) with the
||| `pgStatus` function.
export
pgOpen : HasIO io => (pgUrl: String) -> io Conn
pgOpen url = (map MkConn (primIO $ prim__dbOpen url))

--
-- Close Connection
--
%foreign libpq "PQfinish"
prim__dbClose : Ptr PGconn -> PrimIO ()

||| Close a connection to a Postgres database.
export
pgClose : HasIO io => Conn -> io ()
pgClose (MkConn conn) = primIO $ prim__dbClose conn
                
--
-- Get Connection Status
--
%foreign libpq "PQstatus"
prim__dbStatus : Ptr PGconn -> Int

connectionStatus: Int -> ConnectionStatus
connectionStatus i
    = case i of
        0 => OK
        1 => BAD
        2 => STARTED
        3 => MADE
        4 => AWAITING_RESPONSE
        5 => AUTH_OK
        6 => SETENV
        7 => SSL_STARTUP
        8 => NEEDED
        _ => OTHER i

||| Get the status of a Postgres database connection.
export
pgStatus : Conn -> ConnectionStatus
pgStatus (MkConn conn) = connectionStatus $ prim__dbStatus conn
