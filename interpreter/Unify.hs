module Unify where

import Control.Monad (guard)

import AST

type Subst = Variable -> Term

emptySubst :: Subst
emptySubst = Var

infix 4 |->
(|->) :: Variable -> Term -> Subst
(x |-> tm) y
  | x == y = tm
  | otherwise = Var y

infixr 3 `o`
o :: Subst -> Subst -> Subst
a `o` b = applyTerm a . b

applyTerm :: Subst -> Term -> Term
applyTerm sub (Var x) = sub x
applyTerm sub (Comp cnam tms) = Comp cnam $ map (applyTerm sub) tms

applyStructure :: Subst -> Structure -> Structure
applyStructure sub (Struct pnam tms) = Struct pnam $ map (applyTerm sub) tms

-- find most general unifier
unify :: Term -> Term -> Maybe Subst
unify (Var x) (Var y) = Just $ if x == y then emptySubst else x |-> Var y
unify (Var x) tm2 = guard (x `notElem` varsTerm tm2) >> Just (x |-> tm2)
unify tm1 (Var y) = guard (y `notElem` varsTerm tm1) >> Just (y |-> tm1)
unify (Comp cnam1 tms1) (Comp cnam2 tms2) = guard (cnam1 == cnam2) >> listUnify tms1 tms2

listUnify :: [Term] -> [Term] -> Maybe Subst
listUnify [] [] = Just emptySubst
listUnify [] _ = Nothing
listUnify _ [] = Nothing
listUnify (tm1 : tms1) (tm2 : tms2) = do
  sub1 <- unify tm1 tm2
  sub2 <- listUnify (map (applyTerm sub1) tms1) (map (applyTerm sub1) tms2)
  Just $ sub2 `o` sub1
