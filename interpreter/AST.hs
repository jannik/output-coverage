module AST where

import Data.List (nub)

data Ident = Ident String Int deriving (Eq, Show) -- int allows renaming
data Pred = Pred String [Term] deriving (Eq, Show)
data Term = Var Ident | Comp Pred deriving (Eq, Show)
data Clause = Pred :<-: [Pred] deriving (Show)
type Program = [Clause]
type Query = [Pred]

varsTerm :: Term -> [Ident]
varsTerm (Var i) = [i]
varsTerm (Comp p) = varsPred p

varsPred :: Pred -> [Ident]
varsPred (Pred _ args) = nub $ concatMap varsTerm args

varsQuery :: Query -> [Ident]
varsQuery = nub . concatMap varsPred

renameVars :: Int -> Program -> Program
renameVars = map . renameVarsClause
  where
    renameVarsClause :: Int -> Clause -> Clause
    renameVarsClause lev (p :<-: ps) = renameVarsPred lev p :<-: map (renameVarsPred lev) ps
    
    renameVarsPred :: Int -> Pred -> Pred
    renameVarsPred lev (Pred f args) = Pred f $ map (renameVarsTerm lev) args
    
    renameVarsTerm :: Int -> Term -> Term
    renameVarsTerm lev (Var (Ident s _)) = Var $ Ident s lev
    renameVarsTerm lev (Comp p) = Comp $ renameVarsPred lev p
