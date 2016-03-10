module Prover where

import AST
import Unify

data ProofTree = Done Subst | Choice [ProofTree]

growTree :: Program -> [Pred] -> ProofTree
growTree prog = grow 1 emptySubst
  where
    -- grow current-depth current-substitution remaining-goals
    grow :: Int -> Subst -> [Pred] -> ProofTree
    grow _ sub [] = Done sub
    grow n sub (g : gs) = Choice
      [ grow (n + 1) (sub' `o` sub) (map (applyPred sub') (ps ++ gs))
      | (p :<-: ps) <- renameVars n prog, sub' <- unify (Comp g) (Comp p) ]
      
-- depth-first search
searchDF :: ProofTree -> [Subst]
searchDF (Done sub) = [sub]
searchDF (Choice trs) = concatMap searchDF trs

-- breadth-first search
searchBF :: ProofTree -> [Subst]
searchBF = concatMap result . concat . takeWhile (not . null) . iterate (concatMap children) . (: [])
  where
    result :: ProofTree -> [Subst]
    result (Done sub) = [sub]
    result (Choice _) = []
    
    children :: ProofTree -> [ProofTree]
    children (Done _) = []
    children (Choice trs) = trs

prove :: Program -> [Pred] -> [Subst]
prove prog = searchDF . growTree prog
