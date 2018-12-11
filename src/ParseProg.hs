module ParseProg where

--

import Language
import Parser
import Basic
import Token
import DerivedPrimitives
import Control.Applicative
import ParseUtility

--

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDefn
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

parseScDefn :: Parser (ScDefn Name)
parseScDefn = do v <- parseVar
                 pf <- many parseVar
                 symbol "="
                 body <- parseExpr
                 return (v, pf, body)

parseExpr :: Parser (Expr Name)
parseExpr =
            do symbol "let" -- local definitions
               ds <- parseSemiColonList parseDef
               symbol "in"
               e <- parseExpr
               return $ ELet NonRecursive ds e
        <|>
            do symbol "let" -- local recdefinitions
               ds <- parseSemiColonList parseDef
               symbol "in"
               e <- parseExpr
               return $ ELet Recursive ds e
        <|>
            do symbol "case" -- case expr
               e <- parseExpr
               symbol "of"
               as <- many parseAlt
               return $ ECase e as
        <|>
            do symbol "\\" -- lambda abstraction
               vs <- many parseVar
               symbol "."
               e <- parseExpr
               return $ ELam vs e
        <|>
            do a <- parseAExpr -- atomic expr
               return a
        <|>
            do e <- parseExpr -- infix binary application
               a <- parseAExpr
               return $ EAp e a
                  <|> return [d]

parseAExpr :: Parser (Expr Name)
parseAExpr = do v <- parseVar -- EVar
                return $ EVar v
         <|> do n <- integer -- ENum
                return $ ENum n
         <|> do c <- parseConstructor -- EConstr
                return c
         <|> do symbol "(" -- parenthesis
                e <- parseExpr
                symbol ")"
                return e

parseConstructor :: Parser (Expr Name)
parseConstructor = do symbol "Pack" -- var can't start with capital letter
                      symbol "{"
                      x <- natural
                      symbol ","
                      y <- natural
                      symbol "}"
                      return $ EConstr x y

parseDef :: Parser (Def Name)
parseDef = do v <- parseVar
              symbol "="
              e <- parseExpr
              return (v, e)

parseVar :: Parser (Name)
parseVar = identifier

parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              n <- natural
              symbol ">"
              vs <- many parseVar
              symbol "->"
              e <- parseExpr
              return (n, vs, e)


