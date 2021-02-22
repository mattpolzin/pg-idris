module Postgres.Data.PostgresValue

import Data.Either
import Data.Strings
import Data.List
import Data.List1
import Postgres.Data.PostgresType
import Language.JSON

public export
data PValue : (pType : PType) -> Type where
  Raw : (rawString : String) -> PValue pType

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

-- OID

export
SafeCast (PValue POid) Integer where
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
[SafeCastList] SafeCast (PValue from) to => SafeCast (PValue (PArray from)) (List to) where
  safeCast (Raw rawString) = (parseArray rawString) >>= (traverse (safeCast . (Raw {pType=from})))

--
-- Default Types
--

public export
data HasDefaultType : Type -> Type where
  DInteger  : SafeCast (PValue PInteger) Integer => HasDefaultType Integer 
  DDouble   : SafeCast (PValue PDouble) Double => HasDefaultType Double
  DChar     : SafeCast (PValue PChar) Char => HasDefaultType Char
  DBoolean  : SafeCast (PValue PBoolean) Bool => HasDefaultType Bool
  DString   : SafeCast (PValue PString) String => HasDefaultType String
  DJson     : SafeCast (PValue PJson) JSON => HasDefaultType JSON
  DList     : HasDefaultType to => HasDefaultType (List to)

safeCastImpl : HasDefaultType to -> (pType ** SafeCast (PValue pType) to)
safeCastImpl (DInteger @{safe}) = (_ ** safe)
safeCastImpl (DDouble @{safe}) = (_ ** safe)
safeCastImpl (DChar @{safe}) = (_ ** safe)
safeCastImpl (DBoolean @{safe}) = (_ ** safe)
safeCastImpl (DString @{safe}) = (_ ** safe)
safeCastImpl (DJson @{safe}) = (_ ** safe)
safeCastImpl (DList @{hdt}) = let (pType1 ** safe) = safeCastImpl hdt
                              in
                                  ((PArray pType1) ** SafeCastList)

public export
data Castable : Type -> Type where
  Cast      : HasDefaultType ty => Castable ty
  CastMaybe : HasDefaultType ty => Castable (Maybe ty)

notNothing : (context : String) -> Maybe String -> Either String String
notNothing ctx = maybeToEither ("Unexpected null when looking for " ++ ctx)

-- some gross repetition between the following two methods.
-- having a bit of brain block figuring out how to combine
-- the two right now.
parse : (pType ** SafeCast (PValue pType) to)
     -> Maybe String 
     -> Either String to
parse (pType ** safe) str = 
  let ctx = (show pType) 
  in
      notNothing ctx str >>= ((maybeToEither $ "Failed to parse " ++ ctx) . safeCast @{safe} . Raw)

parseNullable : (pType ** SafeCast (PValue pType) to)
             -> Maybe String 
             -> Either String (Maybe to)
parseNullable (pType ** safe) str = 
  let ctx = (show pType) ++ "?"
  in
      case str of
        Just s  => Just <$> ((maybeToEither $ "Failed to parse " ++ ctx) $ safeCast @{safe} $ Raw s)
        Nothing => Right Nothing

||| Turn the string coming from Postgres into its default Idris type.
public export
asDefaultType : Castable t -> (columnValue : Maybe String) -> Either String t
asDefaultType (Cast @{hdt}) = parse (safeCastImpl hdt)
asDefaultType (CastMaybe @{hdt}) = parseNullable (safeCastImpl hdt)

