module Main where

--

import Language
import Lib

--

main :: IO (Program Name)
main = do inp <- readF
          return $ run inp