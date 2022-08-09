module Postgres.Data.PostgresValue

import Data.Either
import Data.String
import Data.List
import Data.List1
import Postgres.Data.PostgresType
import Language.JSON

public export
data PValue : (p : PType) -> Type where
  Raw : (rawString : String) -> PValue p

export
rawString : PValue t -> String
rawString (Raw r) = r

||| Represents an ability to cast a Postgres type of value (pt) to
||| a particular Idris type (ty).
||| @param pt A Postgres type to cast from.
||| @param ty An Idris type to cast to.
public export
interface ValueCast pt ty where
  safeCast : PValue pt -> Maybe ty

-- Integer

export
ValueCast PInteger Integer where
  safeCast = parseInteger . rawString

export
ValueCast PInteger Double where
  safeCast = parseDouble . rawString

-- Double

export
ValueCast PDouble Double where
  safeCast = parseDouble . rawString
  
-- Char

export
ValueCast PChar Char where
  safeCast (Raw str) with (unpack str)
    safeCast (Raw str) | [c] = Just c
    safeCast (Raw str) | _ = Nothing

-- Boolean

export
ValueCast PBoolean Bool where
  safeCast (Raw "t")     = Just True
  safeCast (Raw "f")     = Just False
  safeCast (Raw "true")  = Just True
  safeCast (Raw "false") = Just False
  safeCast (Raw "1")     = Just True
  safeCast (Raw "0")     = Just False
  safeCast (Raw _)       = Nothing

-- TODO: Date

-- TODO: Time

-- TODO: Datetime

-- String

export
Cast (PValue PString) String where
  cast = rawString

export
ValueCast PString String where
  safeCast = Just . cast

-- JSON

export
ValueCast PJson JSON where
  safeCast = parse . rawString

-- TODO: UUID

-- OID

export
ValueCast POid Integer where
  safeCast = parseInteger . rawString

-- Arrays

parseArray : String -> Maybe (List String)
parseArray str = 
  let stripped = trim str
  in
      do guard (("{" `isPrefixOf` stripped) && ("}" `isSuffixOf` stripped))
         let middle = reverse . (drop 1) . reverse . (drop 1) $ unpack stripped
         pure . forget $ pack <$> (splitOn ',' middle)

export
ValueCast from to => ValueCast (PArray from) (List to) where
  safeCast (Raw rawString) = (parseArray rawString) >>= (traverse (safeCast . (Raw {p=from})))

--
-- Default Types
--

getSafeCast : (t1 : PType) -> ValueCast t1 t2 => ValueCast t1 t2
getSafeCast t1 @{safe} = safe

export
interface DBStringCast a where
  ||| Cast the string representation of a database value to
  ||| the given Idris type if possible or provide an error
  ||| message.  
  dbCast : String -> Either String a

export
DBStringCast Integer where
  dbCast str = maybeToEither "Failed to parse an integer from '\{str}'" . safeCast @{getSafeCast PInteger} $ Raw str

export
DBStringCast Double where
  dbCast str = maybeToEither "Failed to parse a double from '\{str}'" . safeCast @{getSafeCast PDouble} $ Raw str

export
DBStringCast Char where
  dbCast str = maybeToEither "Failed to parse a char from '\{str}'" . safeCast @{getSafeCast PChar} $ Raw str

export
DBStringCast Bool where
  dbCast str = maybeToEither "Failed to parse a boolean from '\{str}'" . safeCast @{getSafeCast PBoolean} $ Raw str

export
DBStringCast String where
  dbCast str = maybeToEither "Failed to parse a string from '\{str}'" . safeCast @{getSafeCast PString} $ Raw str

export
DBStringCast JSON where
  dbCast str = maybeToEither "Failed to parse json from '\{str}'" . safeCast @{getSafeCast PJson} $ Raw str

export
DBStringCast a => DBStringCast (List a) where
  dbCast str = traverse dbCast <=< maybeToEither "Failed to parse an array '\{str}'" $ parseArray str

export
[ValueCastString] ValueCast pt ty => DBStringCast ty where
  dbCast str = maybeToEither "Failed to parse '\{str}' as expected type." . safeCast {pt} $ Raw str

public export
data Castable : Type -> Type where
  Cast      : DBStringCast ty => Castable ty
  CastMaybe : DBStringCast ty => Castable (Maybe ty)

||| Turn the string coming from Postgres into its default Idris type.
|||
||| You can create your own `DBStringCast` implementations if you wish to
||| be able to parse additional types using this library's builtin functions.
public export
asDefaultType : Castable t -> (columnValue : Maybe String) -> Either String t
asDefaultType (Cast      @{dbcast}) = dbCast <=< maybeToEither "Unexpected null while casting Postgres values!"
asDefaultType (CastMaybe @{dbcast}) = traverse dbCast

