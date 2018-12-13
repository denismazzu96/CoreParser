module Token where

--

import Control.Applicative
import Data.Char
import Parser
import DerivedPrimitives

--

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do
             space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int -- not accept string like "- 3" with space between number and sign

symbol :: String -> Parser String
symbol xs = token (string xs)

symbols :: [String] -> Parser String
symbols xs = token (strings xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

ints :: Parser [Int]
ints = do symbol "["
          n <- integer
          ns <- many (do symbol ","
                         integer)
          symbol "]"
          return (n:ns)