module Lib where

--

import System.IO
import Parser
import ParseProg
import Language

readF :: IO String
readF = do inh <- openFile "input.txt" ReadMode
           prog <- readloop inh
           hClose inh
           return prog

comp :: [(Program Name, Name)] -> Program Name
comp [] = error "no parse"
comp [(e, [])] = e
comp [(_, a)] = error ("doesn't use all input"++a)

readloop inh = do ineof <- hIsEOF inh
                  if ineof
                    then return []
                    else do
                      x <- hGetLine inh
                      xs <- readloop inh
                      return (x ++ xs)


run :: String -> Program Name
run inp = comp (parse parseProg inp)