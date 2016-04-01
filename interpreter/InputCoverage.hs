module InputCoverage where

import Control.Monad (guard)
import Data.List (find)
import Data.Maybe (isJust)

import AST
import Unify (unify)

maxDepth :: Mode -> [Clause] -> Int
maxDepth mo cls = maximum (0 : map maxDepth' cls)
  where
    maxDepth' :: Clause -> Int
    maxDepth' (Clause _ (Struct _ tms) _) = maximum $ 0 : [ termDepth tm | (In, tm) <- zip mo tms ]

findSplittingVariable :: Int -> [Term] -> [(ConName, TypeName, Int)] -> Maybe (ConName, TypeName, Int)
findSplittingVariable maxDpth [] svars = Nothing
findSplittingVariable maxDpth ((Var _) : tms) svars = findSplittingVariable maxDpth tms svars
findSplittingVariable maxDpth ((Comp cnam []) : tms) svars =
  case find (\(cnam', _, _) -> cnam' == cnam) svars of
    Nothing -> findSplittingVariable maxDpth tms svars
    Just (svar@(_, _, currDpth))
      | currDpth < maxDpth -> Just svar
      | otherwise -> findSplittingVariable maxDpth tms svars
findSplittingVariable maxDpth ((Comp cnam tms') : tms) svars = findSplittingVariable maxDpth (tms' ++ tms) svars

genTerms :: Int -> Int -> [Constructor] -> (Int, [(ConName, TypeName, Int)], [Term])
genTerms i _ [] = (i, [], [])
genTerms i currDepth (cn : cns) =
  let (i', svars, tm) = genTerm i currDepth cn
      (i'', svars', tms) = genTerms i' currDepth cns
  in (i'', svars ++ svars', tm : tms)

genTerm :: Int -> Int -> Constructor -> (Int, [(ConName, TypeName, Int)], Term)
genTerm i currDepth (Con cnam tpSig) =
  let (i', cnams) = makeSplittingVariables i (length tpSig) []
  in (i', zip3 cnams tpSig (repeat (currDepth + 1)), Comp cnam (map (\cnam' -> Comp cnam' []) cnams))

makeSplittingVariables :: Int -> Int -> [ConName] -> (Int, [ConName])
makeSplittingVariables i 0 acc = (i, acc)
makeSplittingVariables i len acc = makeSplittingVariables (i + 1) (len - 1) (acc ++ ["_" ++ show i])

instSplittingVariable :: ConName -> [Term] -> Term -> [Term]
instSplittingVariable cnam tms tm = map instSV tms
  where
    instSV :: Term -> Term
    instSV (Var x) = Var x
    instSV (Comp cnam' []) | cnam' == cnam = tm
    instSV (Comp cnam' tms') = Comp cnam' $ map instSV tms'

-- first (and last) int is fresh number supply, second int is max split depth
-- conname ints are current split depths; switch to monadic approach
split :: Int -> Int -> [Type] -> [(ConName, TypeName, Int)] -> Structure -> [Structure] -> Maybe (Int, [(ConName, TypeName, Int)], [Structure])
split i maxDpth tps svars (Struct pnam tms) strs = do
  (cnam, tnam, currDpth) <- findSplittingVariable maxDpth tms svars
  guard $ currDpth < maxDpth
  (Typ _ cns) <- find (\(Typ tnam' cns) -> tnam' == tnam) tps -- should not fail in a well-typed program
  let (i', svars', tms') = genTerms i currDpth cns
      strs' = map ((Struct pnam) . (instSplittingVariable cnam tms)) tms'
      strs'' = concatMap (\(Struct pnam' tms'') -> map ((Struct pnam') . (instSplittingVariable cnam tms'')) tms') strs
  Just (i', svars ++ svars', strs' ++ strs'')

-- todo: add proper error handling
inputCoverageCheck :: [Type] -> Program -> Bool
inputCoverageCheck tps prog = all checkPredicate prog
  where
    checkPredicate :: Predicate -> Bool
    checkPredicate (Pred pnam tpSig mo cls) =
      let maxDpth = 1 + maxDepth mo cls
          heads = map (\(Clause _ str _) -> str) cls
          (i, svars, tms) = initTerms 0 $ zip mo tpSig
          str = (Struct pnam tms)
      in check i maxDpth tps svars heads [str]

initTerms :: Int -> [(Polarity, TypeName)] -> (Int, [(ConName, TypeName, Int)], [Term])
initTerms i [] = (i, [], [])
initTerms i ((In, tnam) : args) =
  let (i', svars, tms) = initTerms (i + 1) args
      cnam = "_" ++ show i
  in (i', (cnam, tnam, 0) : svars, Comp cnam [] : tms)
initTerms i (_ : args) =
  let (i', svars, tms) = initTerms (i + 1) args
  in (i', svars, Var ("_", i) : tms)

check :: Int -> Int -> [Type] -> [(ConName, TypeName, Int)] -> [Structure] -> [Structure] -> Bool
check _ _ _ _ _ [] = True
check i maxDpth tps svars heads (str : strs) =
  if any (\str' -> isJust (unify (structureToTerm str') (structureToTerm str))) heads
    then check i maxDpth tps svars heads strs
    else case split i maxDpth tps svars str strs of
      Nothing -> False
      Just (i', svars', strs') -> check i' maxDpth tps svars' heads strs'
