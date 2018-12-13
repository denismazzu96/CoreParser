module DerivedPrimitives where

--

import Control.Applicative
import Data.Char
import Parser
import Basic
import Language

--

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

alphanumPlus :: Parser Char
alphanumPlus = sat $ \x -> x == '_' || isAlphaNum x

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

strings :: [String] -> Parser String
strings [] = empty
strings (x:xs) = string x <|> strings xs

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanumPlus
  if any ((x:xs)==) keywords then empty else return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
        <|> nat
