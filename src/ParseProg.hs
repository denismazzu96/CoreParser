module ParseProg where

--

import Language
import Parser
import Basic
import Token
import DerivedPrimitives
import Control.Applicative
import ParseUtility

---------------------------------------------------------------------- Programs

parseProg :: Parser (Program Name)
parseProg = do
  p <- parseScDefn
  do
    symbol ";"
    ps <- parseProg
    return (p:ps)
    <|> return [p]

-------------------------------------------------------------- Supercombinators

parseScDefn :: Parser (ScDefn Name)
parseScDefn = do
  v <- parseVar
  pf <- many parseVar
  symbol "="
  body <- parseExpr
  return (v, pf, body)

------------------------------------------------------------------- Expressions

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
  e <- parseExpr
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

-------------------------------------------------------- Aritmetical Operations

parseEVar :: Parser (Expr Name)
parseEVar = do
  v <- parseVar
  return $ EVar v

parseENum :: Parser (Expr Name)
parseENum = do
  n <- integer
  return $ ENum n

parseConstructor :: Parser (Expr Name)
parseConstructor = do
  symbol "Pack"
  symbol "{"
  x <- natural
  symbol ","
  y <- natural
  symbol "}"
  return $ EConstr x y

parsePar :: Parser (Expr Name)
parsePar = do
  symbol "("
  e <- parseExpr
  symbol ")"
  return e

parseAExpr :: Parser (Expr Name)
parseAExpr = parseEVar <|> parseENum <|> parseConstructor <|> parsePar

------------------------------------------------------------------- Definitions
parseDef :: Parser (Def Name)
parseDef = do
  v <- parseVar
  symbol "="
  e <- parseExpr
  return (v, e)

-------------------------------------------------------------------------- Vars

parseVar :: Parser (Name)
parseVar = identifier

------------------------------------------------------------------ Alternatives

parseAlt :: Parser (Alter Name)
parseAlt = do
  symbol "<"
  n <- natural
  symbol ">"
  vs <- many parseVar
  symbol "->"
  e <- parseExpr
  return (n, vs, e)
