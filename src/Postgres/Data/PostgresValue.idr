module Postgres.Data.PostgresValue

import Data.Either
import Data.Strings
import Postgres.Data.PostgresType
import Language.JSON

public export
data PValue : (pType : PType) -> Type where
  Raw : (rawString : String) -> PValue t

export
rawString : PValue t -> String
rawString (Raw r) = r

public export
interface SafeCast ty1 ty2 where
  safeCast : ty1 -> Maybe ty2

-- Integer

export
SafeCast (PValue PInteger) Integer where
  safeCast = parseInteger . rawString

export
SafeCast (PValue PInteger) Double where
  safeCast = parseDouble . rawString

-- Double

export
SafeCast (PValue PDouble) Double where
  safeCast = parseDouble . rawString
  
-- Char

export
SafeCast (PValue PChar) Char where
  safeCast (Raw str) with (unpack str)
    safeCast (Raw str) | [c] = Just c
    safeCast (Raw str) | _ = Nothing

-- Boolean

export
SafeCast (PValue PBoolean) Bool where
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
Cast (PValue PString) String where
  cast = rawString

export
SafeCast (PValue PString) String where
  safeCast = Just . cast

-- JSON

export
SafeCast (PValue PJson) JSON where
  safeCast = parse . rawString

-- TODO: UUID

--
-- Default Types
--

public export
data HasDefaultType : Type -> Type where
  DInteger  : HasDefaultType Integer 
  DMInteger : HasDefaultType (Maybe Integer)
  DDouble   : HasDefaultType Double
  DMDouble  : HasDefaultType (Maybe Double)
  DChar     : HasDefaultType Char
  DMChar    : HasDefaultType (Maybe Char)
  DBoolean  : HasDefaultType Bool
  DMBoolean : HasDefaultType (Maybe Bool)
  DString   : HasDefaultType String
  DMString  : HasDefaultType (Maybe String)
  DJson     : HasDefaultType JSON
  DMJson    : HasDefaultType (Maybe JSON)

notNothing : Maybe String -> Either String String
notNothing = maybeToEither "Unexpected null"

-- some gross repetition between the following two methods.
-- having a bit of brain block figuring out how to combine
-- the two right now.
parse : {t : _} 
     -> SafeCast (PValue t) to 
     => (context : String) 
     -> Maybe String 
     -> Either String to
parse ctx str = 
  notNothing str >>= ((maybeToEither $ "Failed to parse " ++ ctx) . safeCast . Raw {t})

parseNullable : {t : _} 
             -> SafeCast (PValue t) to 
             => (context : String) 
             -> Maybe String 
             -> Either String (Maybe to)
parseNullable ctx str = 
  case str of
    Just s  => Just <$> ((maybeToEither $ "Failed to parse " ++ ctx) $ safeCast $ Raw {t} s)
    Nothing => Right Nothing

||| Turn the string coming from Postgres into its default Idris type.
public export
asDefaultType : HasDefaultType t -> (columnValue : Maybe String) -> Either String t
asDefaultType DInteger  = parse "Integer" {t=PInteger}
asDefaultType DDouble   = parse "Double"  {t=PDouble}
asDefaultType DChar     = parse "Char"    {t=PChar}
asDefaultType DBoolean  = parse "Boolean" {t=PBoolean}
asDefaultType DString   = parse "String"  {t=PString}
asDefaultType DJson     = parse "JSON"    {t=PJson}
asDefaultType DMInteger = parseNullable "Integer?" {t=PInteger}
asDefaultType DMDouble  = parseNullable "Double?"  {t=PDouble}
asDefaultType DMChar    = parseNullable "Char?"    {t=PChar}
asDefaultType DMBoolean = parseNullable "Boolean?" {t=PBoolean}
asDefaultType DMString  = parseNullable "String?"  {t=PString}
asDefaultType DMJson    = parseNullable "JSON?"    {t=PJson}

