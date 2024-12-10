module Postgres.Data.PostgresValue

import Data.Either
import Data.String
import Data.List
import Data.List1
import Postgres.Data.PostgresType
import JSON.Parser

public export
data PValue : (p : PType) -> Type where
  Raw : (rawString : String) -> PValue p

export
Semigroup (PValue p) where
  (Raw x) <+> (Raw y) = Raw (x <+> y)

export
Monoid (PValue p) where
  neutral = Raw ""

export
rawString : PValue t -> String
rawString (Raw r) = r

||| Represents an ability to cast a Postgres type of value (pt) to
||| a particular Idris type (ty).
||| @param pt A Postgres type to cast from.
||| @param ty An Idris type to cast to.
public export
interface IdrCast pt ty where
  toIdris : PValue pt -> Maybe ty

public export
interface PGCast pt ty where
  toPostgres : ty -> PValue pt

-- Integer

export
IdrCast PInteger Int where
  toIdris = parseInteger . rawString

export
PGCast PInteger Int where
  toPostgres = Raw . show

export
IdrCast PInteger Integer where
  toIdris = parseInteger . rawString

export
PGCast PInteger Integer where
  toPostgres = Raw . show

export
IdrCast PInteger Double where
  toIdris = parseDouble . rawString

export
PGCast PInteger Nat where
  toPostgres = Raw . show

-- Double

export
IdrCast PDouble Double where
  toIdris = parseDouble . rawString
  
export
PGCast PDouble Double where
  toPostgres = Raw . show

export
PGCast PDouble Integer where
  toPostgres = Raw . show

export
PGCast PDouble Int where
  toPostgres = Raw . show

-- Char

export
IdrCast PChar Char where
  toIdris (Raw str) with (unpack str)
    toIdris (Raw str) | [c] = Just c
    toIdris (Raw str) | _ = Nothing

export
PGCast PChar Char where
  toPostgres c = Raw $ "'\{String.singleton c}'"

-- Boolean

export
IdrCast PBoolean Bool where
  toIdris (Raw "t")     = Just True
  toIdris (Raw "f")     = Just False
  toIdris (Raw "true")  = Just True
  toIdris (Raw "false") = Just False
  toIdris (Raw "1")     = Just True
  toIdris (Raw "0")     = Just False
  toIdris (Raw _)       = Nothing

export
PGCast PBoolean Bool where
  toPostgres True = Raw "true"
  toPostgres False = Raw "false"

-- TODO: Date

-- TODO: Time

-- TODO: Datetime

-- String

export
Cast (PValue PString) String where
  cast = rawString

export
IdrCast PString String where
  toIdris = Just . cast

export
PGCast PString String where
  toPostgres s = Raw $ "'\{s}'"

-- JSON

export
IdrCast PJson JSON where
  toIdris json =
    eitherToMaybe $ parseJSON Virtual (rawString json)

export
PGCast PJson JSON where
  toPostgres j = Raw $ "'\{show j}'"

-- TODO: UUID

-- OID

export
IdrCast POid Integer where
  toIdris = parseInteger . rawString

export
PGCast POid Integer where
  toPostgres = Raw . show

-- Arrays

parseArray : String -> Maybe (List String)
parseArray str = 
  let stripped = trim str
  in
      do guard (("{" `isPrefixOf` stripped) && ("}" `isSuffixOf` stripped))
         let middle = reverse . (drop 1) . reverse . (drop 1) $ unpack stripped
         pure . forget $ pack <$> (splitOn ',' middle)

export
IdrCast from to => IdrCast (PArray from) (List to) where
  toIdris (Raw rawString) = (parseArray rawString) >>= (traverse (toIdris . (Raw {p=from})))

export
PGCast to from => PGCast (PArray to) (List from) where
  toPostgres xs = 
    let values = concat . intersperse "," $ escaped . toPostgres {pt=to} <$> xs
    in Raw $ "'{" ++ values ++ "}'"
    where
      escaped : PValue t -> String
      escaped (Raw str) = if "'" `isPrefixOf` str
                             then "'\{str}'"
                             else str

-- Maybe

export
PGCast to (Maybe from) => PGCast to from where
  toPostgres = toPostgres . Just

--
-- Default Types
--

getSafeCast : (t1 : PType) -> IdrCast t1 t2 => IdrCast t1 t2
getSafeCast t1 @{safe} = safe

export
interface DBStringCast a where
  ||| Cast the string representation of a database value to
  ||| the given Idris type if possible or provide an error
  ||| message.  
  dbCast : String -> Either String a

export
DBStringCast Integer where
  dbCast str = maybeToEither "Failed to parse an integer from '\{str}'" . toIdris @{getSafeCast PInteger} $ Raw str

export
DBStringCast Double where
  dbCast str = maybeToEither "Failed to parse a double from '\{str}'" . toIdris @{getSafeCast PDouble} $ Raw str

export
DBStringCast Char where
  dbCast str = maybeToEither "Failed to parse a char from '\{str}'" . toIdris @{getSafeCast PChar} $ Raw str

export
DBStringCast Bool where
  dbCast str = maybeToEither "Failed to parse a boolean from '\{str}'" . toIdris @{getSafeCast PBoolean} $ Raw str

export
DBStringCast String where
  dbCast str = maybeToEither "Failed to parse a string from '\{str}'" . toIdris @{getSafeCast PString} $ Raw str

export
DBStringCast JSON where
  dbCast str = maybeToEither "Failed to parse json from '\{str}'" . toIdris @{getSafeCast PJson} $ Raw str

export
DBStringCast a => DBStringCast (List a) where
  dbCast str = traverse dbCast <=< maybeToEither "Failed to parse an array '\{str}'" $ parseArray str

export
[IdrCastString] IdrCast pt ty => DBStringCast ty where
  dbCast str = maybeToEither "Failed to parse '\{str}' as expected type." . toIdris {pt} $ Raw str

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

