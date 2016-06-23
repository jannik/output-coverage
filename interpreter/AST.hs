{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module AST where

import Data.List (nub)

type Program a = [Predicate a]
data Predicate a = Pred PredName [Dependency a] Mode [Clause a] deriving (Eq, Show)
type PredName = String
type Dependency a = (a, Type a)
data Type a = Typ TypeName [Term a] deriving (Eq, Ord, Show) -- Ord is hopefully temporary
type TypeName = String
data Clause a = Clause [Dependency a] (Structure a) [Structure a] deriving (Eq, Ord, Show) -- Ord is hopefully temporary
data Structure a = Struct PredName [Term a] deriving (Eq, Ord, Show) -- Ord is hopefully temporary
data Term a = Var a | Comp ConName [Term a] deriving (Eq, Ord, Show) -- Ord is hopefully temporary
type VarName = String
type ConName = String

type Query a = [Structure a]

data Signature a = Sig [TypeFam a] [(PredName, [Dependency a], Mode)] deriving (Eq, Show)
data TypeFam a = TypeFam TypeName [Dependency a] [Constructor a] deriving (Eq, Show)
data Constructor a = Con ConName [Dependency a] (Type a) deriving (Eq, Show)

type Mode = [Polarity]
data Polarity = In | Out | None deriving (Eq, Show)

class PP a where
  pp :: a -> String

instance PP String where
  pp = id

instance PP (String, Int) where
  pp = fst

mapProgram :: (a -> b) -> Program a -> Program b
mapProgram f = map (mapPredicate f)

mapPredicate :: (a -> b) -> Predicate a -> Predicate b
mapPredicate f (Pred pnam deps mo cls) = Pred pnam (map (mapDependency f) deps) mo (map (mapClause f) cls)

mapDependency :: (a -> b) -> Dependency a -> Dependency b
mapDependency f (x, tp) = (f x, mapType f tp)

mapType :: (a -> b) -> Type a -> Type b
mapType f (Typ tnam tms) = Typ tnam (map (mapTerm f) tms)

mapClause :: (a -> b) -> Clause a -> Clause b
mapClause f (Clause deps str strs) = Clause (map (mapDependency f) deps) (mapStructure f str) (map (mapStructure f) strs)

mapStructure :: (a -> b) -> Structure a -> Structure b
mapStructure f (Struct pnam tms) = Struct pnam (map (mapTerm f) tms)

mapTerm :: (a -> b) -> Term a -> Term b
mapTerm f (Var x) = Var (f x)
mapTerm f (Comp cnam tms) = Comp cnam (map (mapTerm f) tms)

mapTypeFam :: (a -> b) -> TypeFam a -> TypeFam b
mapTypeFam f (TypeFam tnam deps cns) = TypeFam tnam (map (mapDependency f) deps) (map (mapConstructor f) cns)

mapConstructor :: (a -> b) -> Constructor a -> Constructor b
mapConstructor f (Con cnam deps tp) = Con cnam (map (mapDependency f) deps) (mapType f tp)

varsTerm :: Eq a => Term a -> [a]
varsTerm (Var x) = [x]
varsTerm (Comp _ tms) = nub $ concatMap varsTerm tms

varsQuery :: Eq a => Query a -> [a]
varsQuery = nub . concatMap varsStructure
  where
    varsStructure :: Eq a => Structure a -> [a]
    varsStructure (Struct _ tms) = nub $ concatMap varsTerm tms

structureToTerm :: Structure a -> Term a
structureToTerm (Struct pnam tms) = Comp pnam tms

termDepth :: Term a -> Int
termDepth (Var _) = 0
termDepth (Comp _ tms) = 1 + maximum (0 : map termDepth tms)

-- should not be here
initVar :: VarName -> (VarName, Int)
initVar = flip (,) 0
