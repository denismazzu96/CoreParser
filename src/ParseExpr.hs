module ParseExpr where

  --

import Language
import Parser
import Basic
import Token
import Control.Applicative
import ParseUtility
-- import ParseDef
-- import ParseProg --circular dependencies

parseLocalDefinition :: IsRec -> Parser (Expr Name)
parseLocalDefinition mode = do
  symbol "let"
  ds <- parseSemiColonList parseDef
  symbol "in"
  e <- parseExpr
  return $ ELet mode ds e

parseCase :: Parser (Expr Name)
parseCase = do
  symbol "case"
  e <- parseExpr
  symbol "of"
  as <- many parseAlt
  return $ ECase e as

parseLambda :: Parser (Expr Name)
parseLambda = do
  symbol "\\"
  vs <- many parseVar
  symbol "."
  e <- parseExpr
  return $ ELam vs e

parseAtomic :: Parser (Expr Name)
parseAtomic = do
  a <- parseAExpr
  return a

parseBinaryAp :: Parser (Expr Name)
parseBinaryAp = do
  e <- parseExpr -- infix binary application
  a <- parseAExpr
  return $ EAp e a

parseExpr :: Parser (Expr Name)
parseExpr =
  parseLocalDefinition NonRecursive <|>
  parseLocalDefinition Recursive <|>
  parseCase <|>
  parseLambda <|>
  parseAtomic <|>
  parseBinaryAp