module AST where

data Ident = Ident String Int deriving (Show) -- int allows renaming
data Pred = Pred String [Term] deriving (Show)
data Term = Var Ident | Comp Pred deriving (Show)
data Clause = Pred :<-: [Pred] deriving (Show)
type Program = [Clause]
