module Postgres.Data.PostgresType

-- see src/include/catalog/pg_type.h

-- just going to have to load types from Postgres:
-- https://github.com/elixir-ecto/postgrex/blob/master/lib/postgrex/types.ex

public export
data PType = PInteger
           | PDouble
           | PChar
           | PBoolean
           | PDatetime
           | PString
           | POther String

export
Show PType where
  show PInteger   = "Integer"
  show PDouble    = "Double"
  show PChar      = "Char"
  show PBoolean   = "Boolean"
  show PDatetime  = "DateTime"
  show PString    = "String"
  show (POther x) = "Other: " ++ x

public export
data FormatCode = Text | Binary

export
Show FormatCode where
  show Text   = "Text"
  show Binary = "Binary"

