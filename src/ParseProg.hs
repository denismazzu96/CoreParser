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

parseLocalDefinition :: IsRec -> Parser (CoreExpr)
parseLocalDefinition mode = do
  symbol (if mode == NonRecursive then "let" else "letrec")
  ds <- parseSemiColonList parseDef
  symbol "in"
  e <- parseExpr
  return $ ELet mode ds e

parseCase :: Parser (CoreExpr)
parseCase = do
  symbol "case"
  e <- parseExpr
  symbol "of"
  as <- many parseAlt
  return $ ECase e as

parseLambda :: Parser (CoreExpr)
parseLambda = do
  symbol "\\"
  vs <- many parseVar
  symbol "."
  e <- parseExpr
  return $ ELam vs e

-- atomic expressions is an application with only one subAExp
parseAp :: Parser (CoreExpr)
parseAp = fmap (mkChain) (some parseAExpr)

parseExpr :: Parser (CoreExpr)
parseExpr =
  parseAp <|>
  parseLocalDefinition NonRecursive <|>
  parseLocalDefinition Recursive <|>
  parseCase <|>
  parseLambda

-------------------------------------------------------- Aritmetical Operations

parseEVar :: Parser (CoreExpr)
parseEVar = do
  v <- parseVar
  return $ EVar v

parseENum :: Parser (CoreExpr)
parseENum = do
  n <- integer
  return $ ENum n

parseConstructor :: Parser (CoreExpr)
parseConstructor = do
  symbol "Pack"
  symbol "{"
  x <- natural
  symbol ","
  y <- natural
  symbol "}"
  return $ EConstr x y

parsePar :: Parser (CoreExpr)
parsePar = do
  symbol "("
  e <- parseExpr
  symbol ")"
  return e

parseAExpr :: Parser (CoreExpr)
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
