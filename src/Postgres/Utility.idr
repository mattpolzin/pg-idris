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
