module Postgres.Data.PostgresValue

import Data.Strings
import Postgres.Data.PostgresType
import Language.JSON

public export
data PValue : (postgresType : PType) -> Type where
  Raw : (raw : String) -> PValue t

raw : PValue ty -> String
raw (Raw r) = r

interface CastMaybe ty1 ty2 where
  castMaybe : ty1 -> Maybe ty2

-- Integer

CastMaybe (PValue PInteger) Integer where
  castMaybe  = parseInteger . raw

-- Double

CastMaybe (PValue PDouble) Double where
  castMaybe = parseDouble . raw
  
-- Char

CastMaybe (PValue PChar) Char where
  castMaybe (Raw str) with (unpack str)
    castMaybe (Raw str) | [c] = Just c
    castMaybe (Raw str) | _ = Nothing

-- Boolean

CastMaybe (PValue PBoolean) Bool where
  castMaybe (Raw "t") = Just True
  castMaybe (Raw "f") = Just False
  castMaybe (Raw "true") = Just True
  castMaybe (Raw "false") = Just False
  castMaybe (Raw "1") = Just True
  castMaybe (Raw "0") = Just False
  castMaybe (Raw _) = Nothing

-- TODO: Date

-- TODO: Time

-- TODO: Datetime

-- String

Cast (PValue PString) String where
  cast (Raw raw) = raw

CastMaybe (PValue PString) String where
  castMaybe = Just . cast

-- JSON

CastMaybe (PValue PJson) JSON where
  castMaybe (Raw raw) = parse raw

-- TODO: UUID

