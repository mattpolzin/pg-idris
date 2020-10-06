module Postgres.Query

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.Exec
import Postgres.Result
import Language.JSON

--
-- JSON Results
--

firstRowCol : (Result) -> String
firstRowCol res = dbResultValue res 0 0

export
pgJSONResult : Conn -> (command: String) -> IO (Maybe JSON)
pgJSONResult conn command = withExecResult conn command toJson where
 toJson : Result -> IO (Maybe JSON)
 toJson r = pure $ [ json | pgResultSuccess r, json <- parse $ firstRowCol r ]
                      -- parse $ firstRowCol r
