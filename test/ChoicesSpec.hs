module ChoicesSpec(tests) where

  --

  import TastyInjection
  import Basic
  import Parser
  import Control.Applicative

  --

  tests = [c0, c1]

  c0 = assertT "[Choices] using first choice" [('a', "bc")] (parse (item <|> return 'd') "abc")
  c1 = assertT "[Choices] using second choice" [('d', "abc")] (parse (empty <|> return 'd') "abc")