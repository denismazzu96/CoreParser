module ParseUtility where

--

import Parser
import Token
import Control.Applicative

--

-- at least one element is return
parseList :: Parser a -> String -> Parser [a]
parseList p s = do first <- p
                   do symbol s
                      ds <- parseList p s
                      return (first:ds)
                    <|> return [first]

parseSemiColonList :: Parser a -> Parser [a]
parseSemiColonList p = parseList p ";"