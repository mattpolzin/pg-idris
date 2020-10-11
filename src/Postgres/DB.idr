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
withDB : HasIO io => (pgUrl: String) -> (onOpen: Conn -> io b) -> (onError: ConnectionStatus -> io b) -> io b
withDB pgUrl onOpen onError = do conn <- pgOpen pgUrl
                                 out <- case (pgStatus conn) of
                                          OK => onOpen conn
                                          x => onError x
                                 pgClose conn
                                 pure out

