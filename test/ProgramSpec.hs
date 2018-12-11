
module ProgramSpec(tests) where

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

c0 = assertT "[Program] INTEGRATION TEST" out (parse parseProg p)

p = "f = 3;\ng x y = let z = x in z;\nh x = case (let y = x in y) of\n<1> -> 2\n<2> -> 5"
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

