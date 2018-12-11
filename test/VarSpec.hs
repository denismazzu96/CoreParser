
module VarSpec(tests) where

--

import TastyInjection
import Basic
import Language
import Parser
import DerivedPrimitives
import Control.Applicative
import Token
import ParseProg

--

tests = [
  c0,
  c1
  ]

c0 = assertT "[Var] var name" [("x_2q", "abc")] (parse parseVar "  x_2q abc")
c1 = assertT "[Var] var list" [(["x_2q", "abc", "s2S"], "_s")] (parse (many parseVar) "  x_2q abc s2S _s")