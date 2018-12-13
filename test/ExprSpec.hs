
module ExprSpec(tests) where

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
  c0
  ]

c0 = assertT "[Expr] atomic aexpr" [(EAp (ENum 3) (EVar "abx"),"")] (parse parseAp "3 abx")