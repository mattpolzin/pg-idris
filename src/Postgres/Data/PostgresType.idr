module Postgres.Data.PostgresType

import Data.List

-- see src/include/catalog/pg_type.h

-- just going to have to load types from Postgres:
-- https://github.com/elixir-ecto/postgrex/blob/master/lib/postgrex/types.ex

{-
SELECT t.oid, t.typname from pg_type as t where t.typname in ('char', 'bool', 'text', 'varchar', 'json', 'int2', 'int4', 'int8', 'float4', 'float8', 'time', 'timestamp', 'timestamptz', 'timetz', 'numeric', 'uuid', 'jsonb', 'cstring');
  -}

||| Postgres Oids are just integers that have special
||| internal semantics within Postgres.
public export
data Oid : Type where
  MkOid : Int -> Oid

Eq Oid where
  (MkOid x) == (MkOid y) = x == y

||| Not an exhaustive list of types, just a short list of
||| builtins with explicit support in this library.
public export
data PType = PInteger
           | PDouble
           | PChar
           | PBoolean
           | PDatetime
           | PString
           | POther String
           | PUnknown Oid

export
Show PType where
  show PInteger   = "Integer"
  show PDouble    = "Double"
  show PChar      = "Char"
  show PBoolean   = "Boolean"
  show PDatetime  = "DateTime"
  show PString    = "String"
  show (POther x) = "Other: " ++ x
  show (PUnknown (MkOid oid)) = "Oid: " ++ (show oid)

public export
data FormatCode = Text | Binary

export
Show FormatCode where
  show Text   = "Text"
  show Binary = "Binary"

||| Get an integer representation of an Oid. Probably only useful
||| when actually sending the Oid to Postgres for some reason.
export
oidToInt : Oid -> Int
oidToInt (MkOid x) = x

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

||| Create a dictionary of Postgres types. The dictionary is
||| sorted by Oid for future lookup.
export
typeDictionary : List (Oid, PType) -> TypeDictionary
typeDictionary xs = MkTypeDictionary (sortBy (compare `on` oidToInt . fst) xs)

export
lookup : Oid -> TypeDictionary -> PType
lookup oid dict = maybe (PUnknown oid) id (snd <$> (find ((== oid) . fst) $ dict.types))

