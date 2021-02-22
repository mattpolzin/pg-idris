module Main

import Test.Golden

tests : TestPool
tests = MkTestPool [] [
  "expected_type_query"
]

main : IO ()
main = runner [tests]

