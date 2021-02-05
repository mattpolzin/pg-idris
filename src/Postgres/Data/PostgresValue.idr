module Postgres.Data.PostgresValue

import Data.Strings
import Postgres.Data.PostgresType
import Language.JSON

public export
data PValue : (pType : PType) -> (colType : PColType pType) -> Type where
  Raw : (rawString : String) -> PValue t ct

export
rawString : PValue t ct -> String
rawString (Raw r) = r

public export
interface SafeCast ty1 ty2 where
  safeCast : ty1 -> Maybe ty2

-- Integer

export
SafeCast (PValue PInteger ct) Integer where
  safeCast = parseInteger . rawString

export
SafeCast (PValue PInteger ct) Double where
  safeCast = parseDouble . rawString

-- Double

export
SafeCast (PValue PDouble ct) Double where
  safeCast = parseDouble . rawString
  
-- Char

export
SafeCast (PValue PChar ct) Char where
  safeCast (Raw str) with (unpack str)
    safeCast (Raw str) | [c] = Just c
    safeCast (Raw str) | _ = Nothing

-- Boolean

export
SafeCast (PValue PBoolean ct) Bool where
  safeCast (Raw "t") = Just True
  safeCast (Raw "f") = Just False
  safeCast (Raw "true") = Just True
  safeCast (Raw "false") = Just False
  safeCast (Raw "1") = Just True
  safeCast (Raw "0") = Just False
  safeCast (Raw _) = Nothing

-- TODO: Date

-- TODO: Time

-- TODO: Datetime

-- String

export
Cast (PValue PString ct) String where
  cast = rawString

export
SafeCast (PValue PString ct) String where
  safeCast = Just . cast

-- JSON

export
SafeCast (PValue PJson ct) JSON where
  safeCast = parse . rawString

-- TODO: UUID

--
-- Default Types
--

public export
data HasDefaultType : Type -> Type where
  DInteger : HasDefaultType Integer 

public export
asDefaultType : HasDefaultType t -> String -> Maybe t
asDefaultType DInteger = safeCast . (Raw {ct=(MkColType True PInteger)})

