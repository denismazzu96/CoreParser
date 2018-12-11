module AExprSpec(tests) where

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
  c0 , --
  c1 , --
  c2 , --
  c3 --
  ]

c0 = assertT "[AExpr] variable" [(EVar "name", "abc")] (parse parseAExpr "name abc")
c1 = assertT "[AExpr] number" [(ENum 3, "abc")] (parse parseAExpr "3 abc")
c2 = assertT "[AExpr] constructor" [(EConstr 1 1, "abc")] (parse parseAExpr "Pack{1, 1} abc")
c3 = assertT "[AExpr] parenthesised expr" [(ENum 3, "abc")] (parse parseAExpr "(3) abc")