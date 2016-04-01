module PrettyPrinter where

import Data.List (intercalate)

import AST
import Unify (Subst)

ppTypes :: [Type] -> String
ppTypes = intercalate "\n" . map ppType

ppType :: Type -> String
ppType (Typ tnam cns) = "%data " ++ tnam ++ " = " ++ intercalate " | " (map ppConstructor cns) ++ "."

ppConstructor :: Constructor -> String
ppConstructor (Con cnam tpSig) = intercalate " " (cnam : tpSig)

ppProgram :: Program -> String
ppProgram = intercalate "\n\n" . map ppPredicate

ppPredicate :: Predicate -> String
ppPredicate (Pred pnam tpSig mo cls) =
  pnam ++ " : " ++ intercalate " x " tpSig ++ " -> type.\n"
    ++ "%mode " ++ pnam ++ " " ++ intercalate " " (map ppPolarity mo) ++ ".\n\n"
    ++ intercalate "\n\n" (map ppClause cls)

ppPolarity :: Polarity -> String
ppPolarity In = "+"
ppPolarity Out = "-"
ppPolarity None = "*"

ppClause :: Clause -> String
ppClause (Clause an str strs) = ppAnnotation an ++ intercalate "\n  <- " (map ppStructure (str : strs)) ++ "."

ppAnnotation :: Annotation -> String
ppAnnotation [] = ""
ppAnnotation an = "{" ++ intercalate ", " (map (\(vnam, tnam) -> vnam ++ " : " ++ tnam) an) ++ "}\n"

ppStructure :: Structure -> String
ppStructure (Struct pnam tms) = intercalate " " (pnam : map ppTerm' tms)

ppTerm :: Term -> String
ppTerm (Var x) = ppVariable x
ppTerm (Comp cnam tms) = intercalate " " (cnam : map ppTerm' tms)

ppTerm' :: Term -> String
ppTerm' (Var x) = ppVariable x
ppTerm' (Comp cnam []) = cnam
ppTerm' (Comp cnam tms) = "(" ++ intercalate " " (cnam : map ppTerm' tms) ++ ")"

ppVariable :: Variable -> String
ppVariable (vnam, _) = vnam

ppQuery :: Query -> String
ppQuery strs = "? " ++ intercalate ", " (map ppStructure strs) ++ "."

ppResult :: [Variable] -> [Subst] -> String
ppResult _ [] = "No."
ppResult xs subs = "Yes:\n" ++ intercalate ";\n" (map (ppSubst xs) subs) ++ "."

ppSubst :: [Variable] -> Subst -> String
ppSubst xs sub =
  case [ ppVariable x ++ " = " ++ ppTerm (sub x) | x <- xs, sub x /= Var x ] of
    [] -> "Empty Substitution"
    ss -> intercalate ", " ss
