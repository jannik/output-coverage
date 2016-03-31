module AST where

import Data.List (find, nub)

type Program = [Predicate]
data Predicate = Pred PredName [TypeName] Mode [Clause] deriving (Eq, Show)
type PredName = String
type TypeName = String
data Clause = Clause Annotation Structure [Structure] deriving (Eq, Show)
type Annotation = [(VarName, TypeName)]
type VarName = String
data Structure = Struct PredName [Term] deriving (Eq, Show)
data Term = Var Variable | Comp ConName [Term] deriving (Eq, Show)
type Variable = (VarName, Int)
type ConName = String

type UProgram = [UClause]
data UClause = UClause UStructure [UStructure] deriving (Eq, Show)
data UStructure = UStruct PredName [UTerm] deriving (Eq, Show)
data UTerm = UVar VarName | UComp ConName [UTerm] deriving (Eq, Show)

type Query = [Structure]

data Signature = Sig [Type] [(PredName, [TypeName], Mode)] deriving (Eq, Show)
data Type = Typ TypeName [Constructor] deriving (Eq, Show)
data Constructor = Con ConName [TypeName] deriving (Eq, Show)

type Mode = [Polarity]
data Polarity = In | Out | None deriving (Eq, Show)

structureToTerm :: Structure -> Term
structureToTerm (Struct pnam tms) = Comp pnam tms

initVars :: UStructure -> Structure
initVars (UStruct pnam tms) = Struct pnam $ map initVarsTerm tms
  where
    initVarsTerm :: UTerm -> Term
    initVarsTerm (UVar vnam) = Var (vnam, 0)
    initVarsTerm (UComp cnam tms) = Comp cnam $ map initVarsTerm tms

varsTerm :: Term -> [Variable]
varsTerm (Var x) = [x]
varsTerm (Comp _ tms) = nub $ concatMap varsTerm tms

varsQuery :: Query -> [Variable]
varsQuery = nub . concatMap varsStructure
  where
    varsStructure :: Structure -> [Variable]
    varsStructure (Struct _ tms) = nub $ concatMap varsTerm tms

renameVars :: Int -> Clause -> Clause
renameVars n (Clause an str strs) = Clause an (renameVarsStructure n str) (map (renameVarsStructure n) strs)
  where
    renameVarsStructure :: Int -> Structure -> Structure
    renameVarsStructure n (Struct pnam tms) = Struct pnam $ map (renameVarsTerm n) tms
    
    renameVarsTerm :: Int -> Term -> Term
    renameVarsTerm n (Var (vnam, _)) = Var (vnam, n)
    renameVarsTerm n (Comp cnam tms) = Comp cnam $ map (renameVarsTerm n) tms
