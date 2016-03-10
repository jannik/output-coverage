module Unify where

import AST

type Subst = Ident -> Term

emptySubst :: Subst
emptySubst = Var

infix 4 |->
(|->) :: Ident -> Term -> Subst
(i |-> t) j
  | i == j = t
  | otherwise = Var j

infixr 3 `o`
o :: Subst -> Subst -> Subst
a `o` b = applyTerm a . b

applyTerm :: Subst -> Term -> Term
applyTerm sub (Var i) = sub i
applyTerm sub (Comp p) = Comp $ applyPred sub p

applyPred :: Subst -> Pred -> Pred
applyPred sub (Pred f args) = Pred f $ map (applyTerm sub) args

-- find most general unifier (result is either a singleton list or empty)
unify :: Term -> Term -> [Subst]
unify (Var i) (Var j) = [if i == j then emptySubst else i |-> Var j]
unify (Var i) t2 = [ i |-> t2 | i `notElem` varsTerm t2 ]
unify t1 (Var j) = [ j |-> t1 | j `notElem` varsTerm t1 ]
unify (Comp (Pred f1 args1)) (Comp (Pred f2 args2)) = [ sub | f1 == f2, sub <- listUnify args1 args2 ]

listUnify :: [Term] -> [Term] -> [Subst]
listUnify [] [] = [emptySubst]
listUnify [] _ = []
listUnify _ [] = []
listUnify (t1 : ts1) (t2 : ts2) =
  [ sub2 `o` sub1 | sub1 <- unify t1 t2
                  , sub2 <- listUnify (map (applyTerm sub1) ts1) (map (applyTerm sub1) ts2) ]
