module Postgres.Query

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.Exec
import Postgres.Result
import Language.JSON

--
-- JSON Results
--

||| Get the first row first column from the given
||| result. Will return `Nothing` if there are no
||| results. 
firstRowCol : (Result) -> Maybe String
firstRowCol res = let (row, col) = pgResultSize res in
                      if row > 0 && col > 0
                         then Just $ dbResultValue res 0 0
                         else Nothing

export
pgJSONResult : Conn -> (command: String) -> IO (Maybe JSON)
pgJSONResult conn command = withExecResult conn command toJson where
 toJson : Result -> IO (Maybe JSON)
 toJson r = pure $ [ json | pgResultSuccess r, json <- parse !(firstRowCol r) ]

