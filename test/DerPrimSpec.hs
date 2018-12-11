module DerPrimSpec(tests) where

  --

  import TastyInjection
  import Basic
  import Parser
  import DerivedPrimitives
  import Control.Applicative

  --

  tests = [c0, c1, c2, c3, c4, c5, c6, c7]

  c0 = assertT "[Derived Primitives] char 'a'" [('a', "bc")] (parse (char 'a') "abc")
  c1 = assertT "[Derived Primitives] string \"abc\"" [("abc", "def")] (parse (string "abc") "abcdef")
  c2 = assertT "[Derived Primitives] string \"abc\" but not found in input" [] (parse (string "abc") "ab1cdef")
  c3 = assertT "[Derived Primitives] many digit" [("123", "abc")] (parse (many digit) "123abc")
  c4 = assertT "[Derived Primitives] many digit in alpha string" [("", "abc")] (parse (many digit) "abc")
  c5 = assertT "[Derived Primitives] some digit in alpha string" [] (parse (some digit) "abc")
  c6 = assertT "[Derived Primitives] many digit in alpha string" [("", "abc")] (parse (many digit) "abc")
  c7 = assertT "[Derived Primitives] identifier" [("ab_2c", " abc")] (parse ident "ab_2c abc")