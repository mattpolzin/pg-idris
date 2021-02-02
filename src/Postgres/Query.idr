module Postgres.Query

import public Language.JSON

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.Data.PostgresType
import Postgres.Data.PostgresValue
import Postgres.Exec
import Postgres.Result
import Data.Fin
import Data.Vect
import Data.HVect

%default total

||| Get the first row first column from the given
||| result. Will return `Nothing` if there are no
||| results. 
maybeFirstRowCol : {r,c : Nat} -> TupleResult r c -> Maybe String
maybeFirstRowCol res = do row <- natToFin 0 r
                          col <- natToFin 0 c
                          pure $ pgResultStringValue res row col

--
-- Headers
--

headerNames : {cols : Nat} -> TupleResult rows cols -> Vect cols String
headerNames t = pgResultsetColNames t

headerTypes : {auto types : TypeDictionary} 
           -> {cols : Nat} 
           -> TupleResult rows cols 
           -> Vect cols PType
headerTypes t = pgResultsetColTypes t

headers : {auto types : TypeDictionary} 
       -> {cols : Nat} 
       -> TupleResult rows cols 
       -> Vect cols ColHeader
headers t = (uncurry MkHeader) <$> (zip (headerNames t) (headerTypes t))

--
-- Stringy (unityped) Results
--

rows : {rows, cols : Nat} -> TupleResult rows cols -> Vect rows (Vect cols String)
rows t = (map force) <$> pgStringResultset t

export
pgStringResultsQuery : {auto types : TypeDictionary} 
                    -> (header : Bool) 
                    -> (query : String) 
                    -> Conn 
                    -> IO (Either String (StringResultset header))
pgStringResultsQuery header query conn = withExecResult conn query stringResults 
  where 
    stringResults : Result -> IO (Either String (StringResultset header))
    stringResults r = do Nothing <- pure $ pgResultErrorMessage r
                           | Just err => pure $ Left err
                         Just tuples <- pure $ tupleResult r
                           | Nothing  => pure $ Left "query did not result in expected response."
                         pure $ Right $ if header
                                        then (_ ** _ ** (headers tuples, rows tuples))
                                        else (_ ** _ ** rows tuples)

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

--
-- Expected Type Results
--

-- Here you pass the vect of types you expect and get back results if possible.

pgResultQuery : {auto types : TypeDictionary} 
             -> (query : String) 
             -> (expected : Vect cols Type) 
             -> Conn 
             -> IO (Either String (HVect expected))
pgResultQuery query expected conn = ?pgResultQuery_rhs

