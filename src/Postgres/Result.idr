module Postgres.Result

import Postgres.Utility
import Postgres.Data.ResultStatus
import Data.Vect
import Data.Fin

||| Internal phantom type used to mark pointers to the
||| `libpq` struct of the same name.
export 
data PGresult : Type

%foreign libpq "PQclear"
prim__dbClearResult : Ptr PGresult -> PrimIO ()

||| The result of executing a command. See `Postgres.Exec`
||| for more.
export
data Result : Type where
  MkResult : Ptr PGresult -> Result


||| All of the various forms of execution supported by libpq
||| provide types that conform to the `ResultProducer` interface.
public export
interface ResultProducer source input where
  exec : source -> input -> IO (Ptr PGresult)

||| Execute the given result producer, do the given thing with the
||| result, and then free the memory used for the result.
export
withResult : ResultProducer source a => source -> a -> (Result -> IO b) -> IO b
withResult src cmd f = do resPtr <- exec src cmd
                          out <- f $ MkResult resPtr
                          primIO $ prim__dbClearResult resPtr
                          pure out


--
-- Result Status
--

resultStatus : Int -> ResultStatus
resultStatus i =
  case i of
       0 => EMPTY_QUERY
       1 => COMMAND_OK
       2 => TUPLES_OK
       3 => COPY_OUT
       4 => COPY_IN
       5 => BAD_RESPONSE
       6 => NONFATAL_ERROR
       7 => FATAL_ERROR
       8 => COPY_BOTH
       9 => SINGLE_TUPLE
       x => OTHER x

%foreign libpq "PQresultStatus"
prim__dbResultStatus : Ptr PGresult -> Int

export
pgResultStatus : Result -> ResultStatus
pgResultStatus (MkResult res) = resultStatus $ prim__dbResultStatus res

export
pgResultSuccess : Result -> Bool
pgResultSuccess res with (pgResultStatus res)
  pgResultSuccess res | EMPTY_QUERY = True
  pgResultSuccess res | COMMAND_OK = True
  pgResultSuccess res | TUPLES_OK = True
  pgResultSuccess res | COPY_OUT = True
  pgResultSuccess res | COPY_IN = True
  pgResultSuccess res | COPY_BOTH = True
  pgResultSuccess res | SINGLE_TUPLE = True
  pgResultSuccess res | x = False

%foreign libpq "PQresultErrorMessage"
prim__dbResultErrorMessage : Ptr PGresult -> String

||| Get the Postgres error message for the given result or
||| Nothing if the result was a success.
export
pgResultErrorMessage : Result -> Maybe String
pgResultErrorMessage result@(MkResult res) = case pgResultSuccess result of
                                                  False => Just $ prim__dbResultErrorMessage res
                                                  True  => Nothing

--
-- Tuple Result
--

%foreign libpq "PQntuples"
prim__dbResultRowCount : Ptr PGresult -> Int

%foreign libpq "PQnfields"
prim__dbResultColCount : Ptr PGresult -> Int

%foreign libpq "PQfname"
prim__dbResultColName : Ptr PGresult -> (col : Int) -> String

%foreign libpq "PQgetisnull"
prim__dbResultColIsNull : Ptr PGresult -> (row : Int) -> (col : Int) -> Int

%foreign libpq "PQfformat"
prim__dbResultColFormatCode : Ptr PGresult -> (col : Int) -> Int

||| Get the type of the column. The resulting Int is a Postgres OID.
||| Built-in data types are defined in the file src/include/catalog/pg_type.h
%foreign libpq "PQftype"
prim__dbResultColType : Ptr PGresult -> (col : Int) -> Int

||| Assumes the result is of type TUPLES_OK. Used
||| internally to define TupleResult or else it
||| would require TupleResult itself.
resultRowCount : Result -> Nat
resultRowCount (MkResult res) = integerToNat $ cast $ prim__dbResultRowCount res

||| Assumes the result is of type TUPLES_OK. Used
||| internally to define TupleResult or else it
||| would require TupleResult itself.
resultColCount : Result -> Nat
resultColCount (MkResult res) = integerToNat $ cast $ prim__dbResultColCount res

resultColName : Result -> (col : Nat) -> String
resultColName (MkResult res) col = prim__dbResultColName res (cast $ natToInteger col)

export 
data TupleResult : (rows: Nat) -> (cols: Nat) -> Type where
  MkTupleResult : (res : Result) -> TupleResult (resultRowCount res) (resultColCount res)

export
tupleResult : (res: Result) -> Maybe (TupleResult (resultRowCount res) (resultColCount res))
tupleResult res = case (pgResultStatus res) of
                       TUPLES_OK => pure $ MkTupleResult res 
                       _ => Nothing

--
-- Result Value
--

||| Get the size of the resultset as (row, col).
export 
pgResultSize : {r: Nat} -> {c: Nat} -> TupleResult r c -> (Nat, Nat)
pgResultSize res = (r, c)

%foreign libpq "PQgetvalue"
prim__dbResultValue : Ptr PGresult -> (row: Int) -> (col: Int) -> String

||| Get the result value at the given row and column as a String regardless
||| of what the underlying Postgres type is.
export
pgResultStringValue : (res: TupleResult rows cols) -> (row: Fin rows) -> (col: Fin cols) -> String
pgResultStringValue (MkTupleResult (MkResult res)) row col = let r = cast $ the Nat (cast row)
                                                                 c = cast $ the Nat (cast col) in 
                                                                 prim__dbResultValue res r c

--
-- Resultset
--

resultRow : {cols: Nat} -> (res : TupleResult rows cols) -> (row: Fin rows) -> (Vect cols (Lazy String))
resultRow res row = valueAt <$> (range {len=cols}) where
  valueAt : Fin cols -> Lazy String
  valueAt col = pgResultStringValue res row col


||| Get the resultset (all rows and columns) with all values as Strings
||| regardless of the underlying Postgres value type.
export
pgStringResultset : {rows: Nat} -> {cols: Nat} -> (res : TupleResult rows cols) -> (Vect rows (Vect cols (Lazy String)))
pgStringResultset res = valueAt <$> (range {len=rows}) where
  valueAt : Fin rows -> Vect cols (Lazy String)
  valueAt = resultRow res


