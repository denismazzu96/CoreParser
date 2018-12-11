
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

c0 = assertT "[Expr] atomic aexpr" [(ENum 3, "abx")] (parse parseExpr "3 abx")