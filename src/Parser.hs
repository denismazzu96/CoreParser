module Parser where

--

import Control.Applicative

newtype Parser a = P(String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

--

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\input -> case parse p input of
    []         -> []
    [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\input -> [(v, input)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\input -> case parse pg input of
    []         -> []
    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input -> case parse p input of
    []         -> []
    [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser
  p <|> q = P (\input -> case parse p input of
    []         -> parse q input
    [(v, out)] -> [(v, out)])
