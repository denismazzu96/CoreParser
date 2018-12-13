module ParseUtility where

--

import Parser
import Language
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

parseParenthesised :: Parser a -> Parser a
parseParenthesised p = do
  symbol "("
  e <- p
  symbol ")"
  return e

parseBoolop :: Parser CoreExpr -> String -> Parser CoreExpr
parseBoolop pNext op = do
  a <- pNext
  do
    symbol op
    b <- parseBoolop pNext op
    return $ EAp (EAp (EVar op) a) b
    <|> return a

parseRelop :: Parser CoreExpr -> [String] -> Parser CoreExpr
parseRelop pNext ops = do
  a <- pNext
  do
    op <- symbols ops
    b <- pNext
    return $ EAp (EAp (EVar op) a) b
    <|> return a



--

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

-- right hand sides of
rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

--

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

--

preludeDefs :: CoreProgram
preludeDefs = [
    ("I", ["x"], EVar "x"),
    ("K", ["x","y"], EVar "x"),
    ("K1",["x","y"], EVar "y"),
    ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x"))),
    ("compose", ["f","g","x"], EAp (EVar "f")
                                   (EAp (EVar "g") (EVar "x"))),
    ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]

--

mkChain :: [CoreExpr] -> CoreExpr
mkChain (x:[]) = x
mkChain xs = EAp (mkChain $ init xs) (last xs)