module BasicSpec(tests) where

--

import TastyInjection
import Basic
import Parser

--

tests = [c0, c1]

c0 = assertT "[Basic] retrieve first item with empty string" [] (parse item "")
c1 = assertT "[Basic] retrieve first item item in \"abc\"" [('a', "bc")] (parse item "abc")