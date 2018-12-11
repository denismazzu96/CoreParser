module Basic where

--

import Parser

--

item :: Parser Char
item = P (\input -> case input of
  []     -> []
  (x:xs) -> [(x, xs)])