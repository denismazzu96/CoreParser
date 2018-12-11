module Language where

--

type Name = String -- for string name
type Def a = (a, Expr a) -- for let and letrec
type Alter a = (Int, [a], Expr a) -- for case
data IsRec = NonRecursive | Recursive deriving (Show, Eq) -- for let and letrec

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

type ScDefn a = (Name, [a], Expr a) -- definition similar to alter TODO: combine definition with alter in more general way
type CoreScDefn = ScDefn Name

