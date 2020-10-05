module Postgres.DB

import Postgres.DB.Core
import Postgres.Data.Conn

--
-- Safe connection
--

||| Open a connection to a Postgres database,
||| perform some actions with it, and then close
||| the connection.
|||
||| You do _not_ call `pgOpen` or `pgClose` directly
||| when using this function.
export
withDB : HasIO io => (pgUrl: String) -> (Conn -> io b) -> io b
withDB pgUrl f = do conn <- pgOpen pgUrl
                    out <- f conn
                    pgClose conn
                    pure out
