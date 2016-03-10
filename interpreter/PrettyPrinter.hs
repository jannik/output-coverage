module PrettyPrinter where

import Data.List (intercalate)

import AST
import Unify (Subst)

newtype Prog = Prog Program

instance Show Prog where
  show (Prog prog) = ppProgram prog

ppProgram :: Program -> String
ppProgram = intercalate "\n\n" . map ppClause

ppClause :: Clause -> String
ppClause (p :<-: ps) = intercalate "\n  <- " (map ppPred (p : ps)) ++ "."

ppPred :: Pred -> String
ppPred (Pred f args) = intercalate " " (f : map ppTerm args)

ppTerm :: Term -> String
ppTerm (Var i) = ppIdent i
ppTerm (Comp p) = ppPred p

ppIdent :: Ident -> String
ppIdent (Ident s _) = s

ppQuery :: Query -> String
ppQuery ps = "? " ++ intercalate ", " (map ppPred ps) ++ "."

ppResult :: [Ident] -> [Subst] -> String
ppResult ids [] = "No."
ppResult ids subs = "Yes:\n" ++ intercalate ";\n" (map (ppSubst ids) subs) ++ "."

ppSubst :: [Ident] -> Subst -> String
ppSubst ids sub =
  case [ ppIdent id ++ " = " ++ ppTerm (sub id) | id <- ids, sub id /= Var id ] of
    [] -> "Empty Substitution"
    strs -> intercalate ", " strs
