module Postgres.Query

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.Exec
import Postgres.Result
import Language.JSON
import Data.Fin

--
-- JSON Results
--

||| Get the first row first column from the given
||| result. Will return `Nothing` if there are no
||| results. 
maybeFirstRowCol : {r,c : Nat} -> TupleResult r c -> Maybe String
maybeFirstRowCol res = do row <- natToFin 0 r
                          col <- natToFin 0 c
                          pure $ pgResultStringValue res row col

||| Get the result as JSON if it is 1 row and column that can
||| be successfully parsed as JSON.
export
pgJSONResult : (command: String) -> Conn -> IO (Maybe JSON)
pgJSONResult command conn = withExecResult conn command toJson where
 toJson : Result -> IO (Maybe JSON)
 toJson r = pure $ [ json | json <- parse !(maybeFirstRowCol !(tupleResult r)) ]

