module Postgres.FFI.Utility

import System.FFI

public export
libpq : String -> String
libpq fn = "C:" ++ fn ++ ", libpq"

public export
cHelper : String -> String
cHelper fn = "C:" ++ fn ++ ", libpg-idris"

public export
nLibpq : String -> String
nLibpq fn = "node:lambda:require('@mattpolzin/libpq-bare')." ++ fn

export
%foreign cHelper "string_value"
prim__string_value : Ptr String -> String

