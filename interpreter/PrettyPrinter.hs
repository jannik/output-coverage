module PrettyPrinter where

import Data.List (intercalate)

import AST
import Unify (Subst)

ppTypeFams :: PP a => [TypeFam a] -> String
ppTypeFams = intercalate "\n\n" . map ppTypeFam

ppTypeFam :: PP a => TypeFam a -> String
ppTypeFam (TypeFam tnam deps cns) = "%data " ++ tnam ++ " " ++ ppDependencies deps ++ " =\n    " ++ intercalate "\n  | " (map ppConstructor cns) ++ "."

ppDependencies :: PP a => [Dependency a] -> String
ppDependencies = intercalate " " . map ppDependency

ppDependency :: PP a => Dependency a -> String
ppDependency (x, tp) = "{" ++ pp x ++ " : " ++ ppType tp ++ "}"

ppType :: PP a => Type a -> String
ppType (Typ tnam tms) = intercalate " " (tnam : map ppTerm' tms)

ppConstructor :: PP a => Constructor a -> String
ppConstructor (Con cnam deps tp) = cnam ++ " : " ++ ppDependencies deps ++ " " ++ ppType tp

ppProgram :: PP a => Program a -> String
ppProgram = intercalate "\n\n" . map ppPredicate

ppPredicate :: PP a => Predicate a -> String
ppPredicate (Pred pnam deps mo cls) =
  pnam ++ " : " ++ ppDependencies deps ++ " type.\n"
    ++ "%mode " ++ pnam ++ " " ++ intercalate " " (map ppPolarity mo) ++ ".\n\n"
    ++ intercalate "\n\n" (map ppClause cls)

ppPolarity :: Polarity -> String
ppPolarity In = "+"
ppPolarity Out = "-"
ppPolarity None = "*"

ppClause :: PP a => Clause a -> String
ppClause (Clause deps str strs) = ppDependencies' deps ++ intercalate "\n  <- " (map ppStructure (str : strs)) ++ "."

ppDependencies' :: PP a => [Dependency a] -> String
ppDependencies' [] = ""
ppDependencies' deps = ppDependencies deps ++ "\n"

ppStructure :: PP a => Structure a -> String
ppStructure (Struct pnam tms) = intercalate " " (pnam : map ppTerm' tms)

ppTerm :: PP a => Term a -> String
ppTerm (Var x) = pp x
ppTerm (Comp cnam tms) = intercalate " " (cnam : map ppTerm' tms)

ppTerm' :: PP a => Term a -> String
ppTerm' (Var x) = pp x
ppTerm' (Comp cnam []) = cnam
ppTerm' (Comp cnam tms) = "(" ++ intercalate " " (cnam : map ppTerm' tms) ++ ")"

ppQuery :: PP a => Query a -> String
ppQuery strs = "? " ++ intercalate ", " (map ppStructure strs) ++ "."

ppResult :: (Eq a, PP a) => [a] -> [Subst a] -> String
ppResult _ [] = "No."
ppResult xs subs = "Yes:\n" ++ intercalate ";\n" (map (ppSubst xs) subs) ++ "."

ppSubst :: (Eq a, PP a) => [a] -> Subst a -> String
ppSubst xs sub =
  case [ pp x ++ " = " ++ ppTerm (sub x) | x <- xs, sub x /= Var x ] of
    [] -> "Empty Substitution"
    ss -> intercalate ", " ss
