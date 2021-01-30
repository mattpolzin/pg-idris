module Postgres.Query

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.Exec
import Postgres.Result
import Language.JSON
import Data.Fin
import Data.Vect

||| Get the first row first column from the given
||| result. Will return `Nothing` if there are no
||| results. 
maybeFirstRowCol : {r,c : Nat} -> TupleResult r c -> Maybe String
maybeFirstRowCol res = do row <- natToFin 0 r
                          col <- natToFin 0 c
                          pure $ pgResultStringValue res row col

--
-- Stringy (unityped) Results
--

export
pgStringResultsQuery : (query : String) -> Conn -> IO (Either String StringResultset)
pgStringResultsQuery query conn = withExecResult conn query stringResults where
  stringResults : Result -> IO (Either String StringResultset)
  stringResults r = do Nothing <- pure $ pgResultErrorMessage r
                         | Just err => pure $ Left err
                       Just tuples <- pure $ tupleResult r
                         | Nothing  => pure $ Left "query did not result in expected response."
                       pure $ Right $ (_ ** _ ** pgStringResultset tuples)

--
-- JSON Results
--

||| Get the result as JSON if it is 1 row and column that can
||| be successfully parsed as JSON.
export
pgJSONResultQuery : (query: String) -> Conn -> IO (Maybe JSON)
pgJSONResultQuery query conn = withExecResult conn query toJson where
 toJson : Result -> IO (Maybe JSON)
 toJson r = pure $ [ json | json <- parse !(maybeFirstRowCol !(tupleResult r)) ]

