module SequencingSpec(tests) where

--

import TastyInjection
import Basic
import Parser
import Data.Char

--

tests = [c0, c1]

c0 = assertT "[Sequencing] Functional - upper first char in \"abc\"" [('A', "bc")] (parse (fmap toUpper item) "abc")
c1 = assertT "[Sequencing] Applicative - apply value without consuming input in \"abc\"" [(1, "abc")] (parse (return 1) "abc")