module Postgres.Utility

import System.FFI

public export
libpq : String -> String
libpq fn = "C:" ++ fn ++ ", libpq"

public export
helper : String -> String
helper fn = "C:" ++ fn ++ ", libpg-idris"

export
%foreign helper "string_value"
prim__string_value : Ptr String -> String

||| Turn the Int 0 into False and all other
||| Ints into True.
export
boolValue : Int -> Bool
boolValue 0 = False
boolValue _ = True
