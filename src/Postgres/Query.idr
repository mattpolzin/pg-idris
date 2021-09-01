module Postgres.Query

import public Language.JSON
import public Data.Vect.Quantifiers
import public Postgres.Data.PostgresValue
import public Data.HVect

import Postgres.Utility
import Postgres.Data.Conn
import Postgres.Data.PostgresType
import Postgres.Exec
import Postgres.Result
import Decidable.Equality
import Data.Fin
import Data.Vect
import Data.Either

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

headerNames : {colCount : Nat} -> TupleResult _ colCount -> Vect colCount String
headerNames t = pgResultsetColNames t

headerTypes : {auto types : TypeDictionary} 
           -> {colCount : Nat} 
           -> TupleResult _ colCount 
           -> Vect colCount PType
headerTypes t = pgResultsetColTypes t

headers : {auto types : TypeDictionary} 
       -> {colCount : Nat} 
       -> TupleResult _ colCount 
       -> Vect colCount ColHeader
headers t = (uncurry MkHeader) <$> (zip (headerNames t) (headerTypes t))

--
-- Stringy (unityped) Results
--

rows : {rowCount, colCount : Nat} -> TupleResult rowCount colCount -> Vect rowCount (Vect colCount (Maybe String))
rows t = (map force) <$> pgNullableStringResultset t

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

processValue : Castable expected
            -> Maybe String
            -> Either String expected
processValue hasDefault = (asDefaultType hasDefault)

processCols : {0 expected : Vect colCount Type}
           -> All Castable expected 
           -> Vect colCount (Maybe String)
           -> Either String (HVect expected)
processCols [] [] = Right []
processCols (castable :: castables) (x :: xs) = 
  [| (processValue castable x) :: (processCols castables xs) |]

processRows : {0 expected : Vect colCount Type} 
           -> {auto castable : (All Castable expected)}
           -> Vect rowCount (Vect colCount (Maybe String)) 
           -> Either String (Vect rowCount (HVect expected))
processRows xs = traverse (processCols castable) xs 

||| Get the result as a rows of the expected types of values if possible.
export
pgResultQuery : {auto types : TypeDictionary} 
             -> {colCount : Nat}
             -> (expected : Vect colCount Type) 
             -> (query : String) 
             -> Conn 
             -> {auto castable : (All Castable expected)}
             -> IO (Either String (rowCount ** Vect rowCount (HVect expected)))
pgResultQuery expected query conn = 
  do Right (rowCount ** receivedCols ** strings) <- pgStringResultsQuery False query conn
       | Left err => pure $ Left err
     pure $ 
       case decEq colCount receivedCols of
            (No _) => 
              Left $ columnMismatchError receivedCols
            (Yes correctCols) => 
              (MkDPair rowCount) <$> processRows (rewrite correctCols in strings)
    where
      columnMismatchError : (received : Nat) -> String
      columnMismatchError received = 
        "Wrong number of columns returned. Expected " ++ (show colCount) ++ " but got " ++ (show received)

