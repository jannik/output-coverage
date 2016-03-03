module PrettyPrinter where

import Data.List (intercalate)

import AST

newtype Prog = Prog Program

instance Show Prog where
  show (Prog prog) = ppProgram prog
  
ppProgram :: Program -> String
ppProgram = intercalate "\n\n" . map ppClause

ppClause :: Clause -> String
ppClause (p :<-: ps) = intercalate "\n  <- " (map ppPred (p : ps)) ++ "."

ppPred :: Pred -> String
ppPred (Pred s ts) = intercalate " " (s : map ppTerm ts)

ppTerm :: Term -> String
ppTerm (Var i) = ppIdent i
ppTerm (Comp p) = ppPred p

ppIdent :: Ident -> String
ppIdent (Ident s _) = s
