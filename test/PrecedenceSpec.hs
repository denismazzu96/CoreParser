
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
  c0,
  c1,
  c2
  ]

c0 = assertT "[Precedence] Application" [([("f", [], EAp (EAp (EVar "f") (EVar "g")) (EVar "y"))], "")] (parse parseProg "f = f g y")
c1 = assertT "[Precedence] Arithmetical operations" [([("f", [], e1)], "")] (parse parseProg "f = 0+1*2+3")
c2 = assertT "[Precedence] Boolean operations" [([("f", [], e2)], "")] (parse parseProg "f = 0&0|1&0")

e1 =
  EAp -- (+0)(+((*1)2)3)
    (EAp -- +0
      (EVar "+")
      (ENum 0)
    )
    (EAp -- +((*1)2)3
      (EAp -- +(1*2)
        (EVar "+")
        (EAp -- (*1)2
          (EAp -- *1
            (EVar "*")
            (ENum 1)
          )
          (ENum 2)
        )
      )
      (ENum 3)
    )

e2 =
  EAp
    (EAp
      (EVar "|")
      (EAp
        (EAp
          (EVar "&")
          (ENum 0)
        )
        (ENum 0)
      )
    )
    (EAp
      (EAp
        (EVar "&")
        (ENum 1)
      )
      (ENum 0)
    )