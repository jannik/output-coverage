module Prover where

import Data.List (find)

import AST
import Unify (Subst, o, applyStructure, emptySubst, unify)

data ProofTree = Done Subst | Choice [ProofTree]

growTree :: Program -> [Structure] -> ProofTree
growTree prog = grow 1 emptySubst
  where
    -- grow current-depth current-substitution remaining-goals
    grow :: Int -> Subst -> [Structure] -> ProofTree
    grow _ sub [] = Done sub
    grow n sub (g : gs) = Choice
      [ grow (n + 1) (sub' `o` sub) (map (applyStructure sub') (strs ++ gs))
      | let (Struct pnam _) = g, (Pred pnam' _ _ cls) <- prog, pnam' == pnam
      , (Clause _ str strs) <- map (renameVars n) cls, Just sub' <- [unify (structureToTerm g) (structureToTerm str)] ]

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

prove :: Program -> [Structure] -> [Subst]
prove prog = searchDF . growTree prog
