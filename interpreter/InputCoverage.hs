module InputCoverage where

import Control.Monad (guard)

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State (StateT, evalStateT, get, modify)

import Data.List (find)
import Data.Maybe (isJust)

import AST
import Unify (unify)

type InputCoverageError = String
type Environment = [Type]
type Context = Int
type M = ExceptT InputCoverageError (StateT Context (ReaderT Environment Identity))

runM :: M a -> Environment -> Context -> Either InputCoverageError a
runM m env ctx = runIdentity . flip runReaderT env . flip evalStateT ctx . runExceptT $ m

maxDepth :: Mode -> [Clause] -> Int
maxDepth mo cls = maximum (0 : map maxDepth' cls)
  where
    maxDepth' :: Clause -> Int
    maxDepth' (Clause _ (Struct _ tms) _) = maximum $ 0 : [ termDepth tm | (In, tm) <- zip mo tms ]

findSplittingVariable :: Int -> [Term] -> [(ConName, TypeName, Int)] -> M (ConName, TypeName, Int)
findSplittingVariable maxDpth [] svars = throwError "no splitting variables remain"
findSplittingVariable maxDpth ((Var _) : tms) svars = findSplittingVariable maxDpth tms svars
findSplittingVariable maxDpth ((Comp cnam []) : tms) svars =
  case find (\(cnam', _, _) -> cnam' == cnam) svars of
    Nothing -> findSplittingVariable maxDpth tms svars
    Just (svar@(_, _, currDpth))
      | currDpth < maxDpth -> return svar
      | otherwise -> findSplittingVariable maxDpth tms svars
findSplittingVariable maxDpth ((Comp cnam tms') : tms) svars = findSplittingVariable maxDpth (tms' ++ tms) svars

genTerms :: Int -> [Constructor] -> M ([(ConName, TypeName, Int)], [Term])
genTerms _ [] = return ([], [])
genTerms currDepth (cn : cns) = do
  (svars, tm) <- genTerm currDepth cn
  (svars', tms) <- genTerms currDepth cns
  return (svars ++ svars', tm : tms)

genTerm :: Int -> Constructor -> M ([(ConName, TypeName, Int)], Term)
genTerm currDepth (Con cnam tpSig) = do
  cnams <- makeSplittingVariables (length tpSig) []
  return (zip3 cnams tpSig (repeat (currDepth + 1)), Comp cnam (map (\cnam' -> Comp cnam' []) cnams))

makeSplittingVariables :: Int -> [ConName] -> M [ConName]
makeSplittingVariables 0 acc = return acc
makeSplittingVariables len acc = do
  cnam <- get >>= return . ("_" ++) . show
  modify (+ 1)
  makeSplittingVariables (len - 1) (acc ++ [cnam])

instSplittingVariable :: ConName -> [Term] -> Term -> [Term]
instSplittingVariable cnam tms tm = map instSV tms
  where
    instSV :: Term -> Term
    instSV (Var x) = Var x
    instSV (Comp cnam' []) | cnam' == cnam = tm
    instSV (Comp cnam' tms') = Comp cnam' $ map instSV tms'

-- first int is max split depth, conname ints are current split depths
split :: Int -> [(ConName, TypeName, Int)] -> Structure -> [Structure] -> M ([(ConName, TypeName, Int)], [Structure])
split maxDpth svars (Struct pnam tms) strs = do
  (cnam, tnam, currDpth) <- findSplittingVariable maxDpth tms svars
  guard $ currDpth < maxDpth -- define your own guard that carries a message
  tp <- asks $ find (\(Typ tnam' cns) -> tnam' == tnam)
  guard $ isJust tp -- should not fail in a well-typed program
  let Just (Typ _ cns) = tp -- unsafe
  (svars', tms') <- genTerms currDpth cns
  let strs' = map ((Struct pnam) . (instSplittingVariable cnam tms)) tms'
      strs'' = concatMap (\(Struct pnam' tms'') -> map ((Struct pnam') . (instSplittingVariable cnam tms'')) tms') strs
  return (svars ++ svars', strs' ++ strs'')

-- temp
inputCoverageCheck :: [Type] -> Program -> Bool
inputCoverageCheck tps prog =
  case runM (inputCoverageCheck' prog) tps 0 of
    Left _ -> False
    Right _ -> True

inputCoverageCheck' :: Program -> M ()
inputCoverageCheck' prog = mapM_ checkPredicate prog
  where
    checkPredicate :: Predicate -> M ()
    checkPredicate (Pred pnam tpSig mo cls) = do
      let maxDpth = 1 + maxDepth mo cls
          heads = map (\(Clause _ str _) -> str) cls
      (svars, tms) <- initTerms $ zip mo tpSig
      check maxDpth svars heads [Struct pnam tms]

initTerms :: [(Polarity, TypeName)] -> M ([(ConName, TypeName, Int)], [Term])
initTerms [] = return ([], [])
initTerms ((In, tnam) : args) = do
  cnam <- get >>= return . ("_" ++) . show
  modify (+ 1)
  (svars, tms) <- initTerms args
  return $ ((cnam, tnam, 0) : svars, Comp cnam [] : tms)
initTerms (_ : args) = do
  ident <- get >>= return . (,) "_"
  modify (+ 1)
  (svars, tms) <- initTerms args
  return (svars, Var ident : tms)

check :: Int -> [(ConName, TypeName, Int)] -> [Structure] -> [Structure] -> M ()
check _ _ _ [] = return ()
check maxDpth svars heads (str : strs) =
  if any (\str' -> isJust (unify (structureToTerm str') (structureToTerm str))) heads
    then check maxDpth svars heads strs
    else do
      (svars', strs') <- split maxDpth svars str strs
      check maxDpth svars' heads strs'
