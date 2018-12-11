
module TokenSpec(tests) where

--

import TastyInjection
import Basic
import Parser
import DerivedPrimitives
import Control.Applicative
import Token

--

tests = [c0, c1, c2, c3, c4, c5]

c0 = assertT "[Token] identifier" [("token", "bc")] (parse identifier "   token  bc")
c1 = assertT "[Token] natural" [(3, "def")] (parse natural "  3   def")
c2 = assertT "[Token] integer" [(-2, "def")] (parse integer " -2  def")
c3 = assertT "[Token] matching symbol" [("123", "abc")] (parse (symbol "123") "  123 abc")
c4 = assertT "[Token] list of nats" [([1, 2, 3], "")] (parse nats " [1, 2, 3] ")
c5 = assertT "[Token] list of ints" [([1, -2, -3], "")] (parse ints " [1, -2, -3] ")