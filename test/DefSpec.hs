
module DefSpec(tests) where

--

import TastyInjection
import Basic
import Language
import Parser
import DerivedPrimitives
import Control.Applicative
import Token
import ParseUtility
import ParseProg

--

tests = [
  c0,
  c1
  ]

c0 = assertT "[Def] one instance" [(("name", ENum 3), "abc")] (parse parseDef "name = 3 abc")
c1 = assertT "[Def] list" [([("name", ENum 3), ("x2_S", ENum 2)], "; abc")] (parse (parseSemiColonList parseDef) "name = 3  ; x2_S = 2; abc")