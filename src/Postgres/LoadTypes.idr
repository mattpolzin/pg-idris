module Postgres.LoadTypes

import Postgres.Data.PostgresType
import Postgres.Data.Conn
import Postgres.Query
import Postgres.Result
import Decidable.Equality
import Data.List
import Data.Vect
import Data.Vect.Elem
import Data.String
import Data.String.Extra
import Data.Either

integerTypeStrings : List String
integerTypeStrings = [
    "int2",
    "int4",
    "int8"
  ]

doubleTypeStrings : List String
doubleTypeStrings = [
    "float4",
    "float8",
    "numeric"
  ]

charTypeStrings : List String
charTypeStrings = ["char"]

booleanTypeStrings : List String
booleanTypeStrings = ["bool"]

dateTypeStrings : List String
dateTypeStrings = ["date"]

timeTypeStrings : List String
timeTypeStrings = [
    "time",
    "timetz"
  ]

datetimeTypeStrings : List String
datetimeTypeStrings = [
    "timestamp",
    "timestamptz"
  ]

stringTypeStrings : List String
stringTypeStrings = [
    "text",
    "varchar",
    "cstring",
    "name" -- maybe its own type that is just _castable_ to String?
  ]

jsonTypeStrings : List String
jsonTypeStrings = [
    "json",
    "jsonb"
  ]

uuidTypeStrings : List String
uuidTypeStrings = ["uuid"]

oidTypeStrings : List String
oidTypeStrings = ["oid"]

quote : String -> String
quote str = '\'' <+ str +> '\''

typeQuery : String
typeQuery = "SELECT oid, typname from pg_type where typname in (" ++ queryTypes ++ ")"
  where
    supportedTypes : List String
    supportedTypes = integerTypeStrings 
                  ++ doubleTypeStrings 
                  ++ charTypeStrings
                  ++ booleanTypeStrings
                  ++ dateTypeStrings
                  ++ timeTypeStrings
                  ++ datetimeTypeStrings
                  ++ stringTypeStrings
                  ++ jsonTypeStrings
                  ++ uuidTypeStrings
                  ++ oidTypeStrings

    -- This is a bit fragile but we currently load types in by their common names
    -- and postgres names array types the same as the type the array contains with
    -- a leading underscore.
    queryTypes : String
    queryTypes = join "," $ quote <$> (((strCons '_') <$> supportedTypes) ++ supportedTypes)

parseOid : Maybe String -> Either String Oid
parseOid oid = do str <- maybeToEither "Found null when looking for Oid" oid
                  maybeToEither "Found non-integer Oid" $
                    MkOid <$> parseInteger str

arrayOrNot : (isArray : Bool) -> PType -> PType
arrayOrNot True ty = PArray ty
arrayOrNot False ty = ty

||| Using the groupings of Postgres string names for types that will
||| map to each PType, parse the given string to a PType (or POther)
parseType : String -> PType
parseType type = case isElem True typeSearch of
                      (No _)  => POther type
                      (Yes e) => case elemToFin e of
                                      0  => arrayOrNot (fst typeSpec) PInteger
                                      1  => arrayOrNot (fst typeSpec) PDouble
                                      2  => arrayOrNot (fst typeSpec) PChar
                                      3  => arrayOrNot (fst typeSpec) PBoolean
                                      4  => arrayOrNot (fst typeSpec) PDate
                                      5  => arrayOrNot (fst typeSpec) PTime
                                      6  => arrayOrNot (fst typeSpec) PDatetime
                                      7  => arrayOrNot (fst typeSpec) PString
                                      8  => arrayOrNot (fst typeSpec) PJson
                                      9  => arrayOrNot (fst typeSpec) PUuid
                                      10 => arrayOrNot (fst typeSpec) POid
  where
    ||| First element of tuple is true if the type
    ||| is an array. Second element of tuple is the
    ||| name of the non-array type (because array types
    ||| are named the same as non-array types but with
    ||| a leading underscore).
    typeSpec : (Bool, String)
    typeSpec = if "_" `isPrefixOf` type
                  then (True, drop 1 type)
                  else (False, type)

    typeSearch : Vect ? Bool
    typeSearch = elem (snd typeSpec) <$> 
                   [integerTypeStrings 
                  , doubleTypeStrings 
                  , charTypeStrings
                  , booleanTypeStrings
                  , dateTypeStrings
                  , timeTypeStrings
                  , datetimeTypeStrings
                  , stringTypeStrings
                  , jsonTypeStrings
                  , uuidTypeStrings
                  , oidTypeStrings]

typeResult : Vect 2 (Maybe String) -> Either String (Oid, PType)
typeResult [oid, type] = [(o, parseType t) | o <- parseOid oid, t <- (maybeToEither "Found null when looking for type" type)]

||| Load Postgres types into a type dictionary. This is needed so that future queries
||| can identify the types of columns in responses.
export
pgLoadTypes : HasIO io => Conn -> io (Either String TypeDictionary)
pgLoadTypes conn =
  do Right (r ** 2 ** resultset) <- liftIO $ pgStringResultsQuery {types=empty} False typeQuery conn 
       | Right (_ ** c ** _) => pure $ Left $ "ERROR: expected 2 columns but got " ++ (show c)
       | Left err            => pure $ Left $ "ERROR: " ++ err
     -- could change following to successfully parse a subset of types even when failing on one of them.
     Right types <- pure $ traverse typeResult resultset
       | Left err => pure $ Left $ "ERROR: " ++ err
     pure $ Right $ typeDictionary $ toList types

