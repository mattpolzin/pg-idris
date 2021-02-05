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

rows : {rows, cols : Nat} -> TupleResult rows cols -> Vect rows (Vect cols (Maybe String))
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

processValue : {expected : Type}
            -> HasDefaultType expected
            -> Maybe String
            -> Either String expected
processValue hasDefault = (asDefaultType hasDefault)

processCols : {expected : Vect cols Type}
           -> All HasDefaultType expected 
           -> Vect cols (Maybe String)
           -> Either String (HVect expected)
processCols [] [] = Right []
processCols (x :: y) (z :: xs) = [| (processValue x z) :: (processCols y xs) |]

processRows : {rows, cols, receivedCols : _} 
           -> {expected : Vect cols Type} 
           -> {auto correctCols : receivedCols = cols}
           -> {auto castable : (All HasDefaultType expected)}
           -> Vect rows (Vect receivedCols (Maybe String)) 
           -> Either String (Vect rows (HVect expected))
processRows {cols} xs with (correctCols)
  processRows {cols = receivedCols} xs | Refl = traverse (processCols castable) xs 

||| Get the result as a rows of the expected types of values if possible.
export
pgResultQuery : {auto types : TypeDictionary} 
             -> (query : String) 
             -> {cols : Nat}
             -> (expected : Vect cols Type) 
             -> Conn 
             -> {auto castable : (All HasDefaultType expected)}
             -> IO (Either String (rows ** Vect rows (HVect expected)))
pgResultQuery query {cols} expected conn = 
  do Right (rows ** receivedCols ** strings) <- pgStringResultsQuery False query conn
       | Left err => pure $ Left err
     case decEq receivedCols cols of
          (No _) => 
            pure $ Left $ columnMismatchError receivedCols
          (Yes correctCols) => 
            pure $ (MkDPair rows) <$> processRows strings
    where
      columnMismatchError : (received : Nat) -> String
      columnMismatchError received = 
        "Wrong number of columns returned. Expected " ++ (show cols) ++ " but got " ++ (show received)

