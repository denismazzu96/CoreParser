module Language where

--

type Triple a b = (a, [b], Expr b)
type Name = String -- for string name
type Def a = (a, Expr a) -- for let and letrec
type Alter a = Triple Int a -- for case
data IsRec = NonRecursive | Recursive deriving (Show, Eq) -- for let and letrec
type CoreExpr = Expr Name

data Expr a =
  EVar Name
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet IsRec [Def a] (Expr a)
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show, Eq)

--

type Program a = [ScDefn a]
type CoreProgram = Program Name

--

type ScDefn a = Triple Name a
type CoreScDefn = ScDefn Name

--
keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

relop :: [String]
relop = ["<<", "<=", "==", "~=", ">=", ">>"]
