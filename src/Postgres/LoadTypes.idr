module Postgres.LoadTypes

import Postgres.Data.PostgresType
import Postgres.Data.Conn
import Postgres.Query
import Postgres.Result
import Decidable.Equality
import Data.List
import Data.Vect
import Data.Vect.Elem
import Data.Strings
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
    "cstring"
  ]

jsonTypeStrings : List String
jsonTypeStrings = [
    "json",
    "jsonb"
  ]

uuidTypeStrings : List String
uuidTypeStrings = ["uuid"]

quote : String -> String
quote str = '\'' <+ str +> '\''

typeQuery : String
typeQuery = "SELECT oid, typname from pg_type where typname in (" ++ supportedTypes ++ ")"
  where
    supportedTypes : String
    supportedTypes = join "," $ quote <$> (integerTypeStrings 
                                        ++ doubleTypeStrings 
                                        ++ charTypeStrings
                                        ++ booleanTypeStrings
                                        ++ dateTypeStrings
                                        ++ timeTypeStrings
                                        ++ datetimeTypeStrings
                                        ++ stringTypeStrings
                                        ++ jsonTypeStrings
                                        ++ uuidTypeStrings)

parseOid : String -> Either String Oid
parseOid oid = maybeToEither "Found non-integer Oid" $
                 MkOid <$> parseInteger oid

||| Using the groupings of Postgres string names for types that will
||| map to each PType, parse the given string to a PType (or POther)
parseType : String -> PType
parseType type = case isElem True typeSearch of
                      (No _)  => POther type
                      (Yes e) => case elemToFin e of
                                      0 => PInteger
                                      1 => PDouble
                                      2 => PChar
                                      3 => PBoolean
                                      4 => PDate
                                      5 => PTime
                                      6 => PDatetime
                                      7 => PString
                                      8 => PJson
                                      9 => PUuid
  where
    typeSearch : Vect ? Bool
    typeSearch = elem type <$> [integerTypeStrings 
                              , doubleTypeStrings 
                              , charTypeStrings
                              , booleanTypeStrings
                              , dateTypeStrings
                              , timeTypeStrings
                              , datetimeTypeStrings
                              , stringTypeStrings
                              , jsonTypeStrings
                              , uuidTypeStrings]

typeResult : Vect 2 String -> Either String (Oid, PType)
typeResult [oid, type] = [(o, parseType type) | o <- parseOid oid]

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

--
-- Tests
--

namespace ParseTypeTests
  ||| We don't care if it is possible to reach the Other and Unknown cases (in fact, it is not
  ||| possible to reach the unkown case during parsing types). We do want to know we do not
  ||| have any PTypes that are impossible to parse, though, because that would be a useless
  ||| type.
  |||
  ||| Property: for all PTypes (other that POther and PUknown) there exists a string coming
  |||           from Postgres that will parse as that type.
  testSupportedTypesReachable : (t : PType) 
                             -> Either (Either (x ** t = PUnknown x) (y ** t = POther y)) 
                                       (str ** parseType str = t)
  testSupportedTypesReachable PInteger     = Right (head integerTypeStrings  ** Refl)
  testSupportedTypesReachable PDouble      = Right (head doubleTypeStrings   ** Refl)
  testSupportedTypesReachable PChar        = Right (head charTypeStrings     ** Refl)
  testSupportedTypesReachable PBoolean     = Right (head booleanTypeStrings  ** Refl)
  testSupportedTypesReachable PDate        = Right (head dateTypeStrings     ** Refl)
  testSupportedTypesReachable PTime        = Right (head timeTypeStrings     ** Refl)
  testSupportedTypesReachable PDatetime    = Right (head datetimeTypeStrings ** Refl)
  testSupportedTypesReachable PString      = Right (head stringTypeStrings   ** Refl)
  testSupportedTypesReachable PJson        = Right (head jsonTypeStrings     ** Refl)
  testSupportedTypesReachable PUuid        = Right (head uuidTypeStrings     ** Refl)
  testSupportedTypesReachable (POther y)   = Left $ Right (_ ** Refl)
  testSupportedTypesReachable (PUnknown x) = Left $ Left  (_ ** Refl)

