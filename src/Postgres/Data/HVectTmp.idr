module Postgres.Data.HVectTmp

import public Data.Vect
import public Data.Vect.Quantifiers

-- Temporary because Idris 2's base lib exports a type alias for HList but not
-- HVect

public export
HVect : Vect n Type -> Type
HVect = All id
