module Postgres.Data.PostgresType

import Data.List

%default total

-- see src/include/catalog/pg_type.h

--
-- Oid
--

||| Postgres Oids are just integers that have special
||| internal semantics within Postgres.
public export
data Oid : Type where
  MkOid : Int -> Oid

Eq Oid where
  (MkOid x) == (MkOid y) = x == y

Show Oid where
  show (MkOid oid) = show oid

||| Get an integer representation of an Oid. Probably only useful
||| when actually sending the Oid to Postgres for some reason.
export
oidToInt : Oid -> Int
oidToInt (MkOid x) = x

--
-- Type
--

||| Not an exhaustive list of types, just a short list of
||| builtins with explicit support in this library.
public export
data PType = PInteger
           | PDouble
           | PChar
           | PBoolean
           | PDate
           | PTime
           | PDatetime
           | PString
           | PJson
           | PUuid
           | POther String
           | PUnknown Oid

export
Show PType where
  show PInteger   = "Integer"
  show PDouble    = "Double"
  show PChar      = "Char"
  show PBoolean   = "Boolean"
  show PDate      = "Date"
  show PTime      = "Time"
  show PDatetime  = "DateTime"
  show PString    = "String"
  show PJson      = "JSON"
  show PUuid      = "UUID"
  show (POther x) = "Other: " ++ x
  show (PUnknown (MkOid oid)) = "Oid: " ++ (show oid)

--
-- Format Code
--

public export
data FormatCode = Text | Binary

export
Show FormatCode where
  show Text   = "Text"
  show Binary = "Binary"

--
-- Type Dictionary
--

export
data TypeDictionary : Type where
  MkTypeDictionary : List (Oid, PType) -> TypeDictionary

-- The dot accessor makes it like a record type where the
-- following accessor is public but the constructor is not.
||| A sorted list of type tuples (pairs of Oid and type).
||| Sorted by Oid.
export
(.types) : TypeDictionary -> List (Oid, PType)
(.types) (MkTypeDictionary xs) = xs

||| A sorted list of type tuples (pairs of Oid and type).
||| Sorted by Oid.
export
types : TypeDictionary -> List (Oid, PType)
types (MkTypeDictionary xs) = xs

export
empty : TypeDictionary
empty = MkTypeDictionary []

||| Create a dictionary of Postgres types. The dictionary is
||| sorted by Oid for future lookup.
export
typeDictionary : List (Oid, PType) -> TypeDictionary
typeDictionary xs = MkTypeDictionary (sortBy (compare `on` oidToInt . fst) xs)

||| Find the Postgres Type the given Oid refers to, or
||| admit defeat via PUnknown.
export
lookup : Oid -> TypeDictionary -> PType
lookup oid = maybe (PUnknown oid) id . lookup oid . types

export
Show TypeDictionary where
  show = concat . intersperse "\n" . map showTuple . types
    where
      showTuple : (Oid, PType) -> String
      showTuple (oid, type) = (show oid) ++ ": " ++ (show type)

