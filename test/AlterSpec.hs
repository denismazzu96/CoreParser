
module AlterSpec(tests) where

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
  c0 ,
  c1 ,
  c2 ,
  c3 ,
  c4
  ]

c0 = assertT "[Alter] no var" [((3, [], ENum 3), "")] (parse parseAlt "<3> -> 3")
c1 = assertT "[Alter] one var" [((3, ["x"], ENum 3), "")] (parse parseAlt "<3> x -> 3")
c2 = assertT "[Alter] multiple var" [((3, ["x", "y", "z"], ENum 3), "")] (parse parseAlt "<3> x y z -> 3")
c3 = assertT "[Alter] multiple alter with \"\\n\"" [([(2, [], ENum 2), (3, [], ENum 3)], "")] (parse (some parseAlt) "\n<2> -> 2\n<3> -> 3")
c4 = assertT "[Alter] multiple alter" [([(2, [], ENum 2), (3, [], ENum 3)], "")] (parse (some parseAlt) "<2> -> 2 <3> -> 3")