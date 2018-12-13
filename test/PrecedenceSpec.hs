
module PrecedenceSpec(tests) where

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

c0 = assertT "[Precedence] Application" [([("f", [], EAp (EAp (EVar "f") (EVar "g")) (EVar "y"))], "")] (parse parseProg "f = f g y")

p = "f = 3;\ng x y = let z = x in z;\nh x = case (let y = x in y) of\n<1> -> 2\n<2> -> 5"
-- f = 3;
-- g x y = let z = x in z;
-- h x = case (let y = x in y) of
-- <1> -> 2
-- <2> -> 5
out = [(three, "")]
three = [
  ("f", [], ENum 3),
  (
    "g",
    ["x", "y"],
    ELet
      NonRecursive
      [("z", EVar "x")]
      (EVar "z")
  ),
  (
    "h",
    ["x"],
    ECase
      (
        ELet
          NonRecursive
          [("y", EVar "x")]
          (EVar "y")
      )
      [
        (1, [], ENum 2),
        (2, [], ENum 5)
      ]
  )
  ]

