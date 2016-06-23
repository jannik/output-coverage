module OutputCoverage where

import Control.Monad (guard, liftM)
import Control.Monad.ListM (groupByM) -- cabal install monadlist

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State (StateT, evalStateT, get, modify, put)

import Data.List (find, sort)
import Data.Maybe (isJust)

import AST
import Unify --(Subst, unify)
-- import InputCoverage (split) -- different type for M, so can't really reuse..

data Variable = Normal (VarName, Int) | Splitting Int Int deriving (Eq, Ord, Show) -- Ord is hopefully temporary

type Program' = Program Variable
type Predicate' = Predicate Variable
type Dependency' = Dependency Variable
type Type' = Type Variable
type Clause' = Clause Variable
type Structure' = Structure Variable
type Signature' = Signature Variable
type Term' = Term Variable
type TypeFam' = TypeFam Variable
type Constructor' = Constructor Variable
type Subst' = Subst Variable

type OutputCoverageError = String
type Environment = ([TypeFam'], Program')
type Context = Int
type M = ExceptT OutputCoverageError (StateT Context (ReaderT Environment Identity))

runM :: M a -> Environment -> Context -> Either OutputCoverageError a
runM m env ctx = runIdentity . flip runReaderT env . flip evalStateT ctx . runExceptT $ m

data ClauseTree = CT Structure' [[ClauseTree]] deriving (Eq, Show)

--temp
groupByM' f = groupByM f . sort

{- START GEN-TREE -}

genTree :: Predicate' -> M [[[ClauseTree]]] -- really M [ClauseTree], only without structures in the roots
genTree (Pred _ _ _ cls) = do
  strsss <- fHead cls
  mapM fHead' strsss

fHead :: [Clause'] -> M [[[Structure']]]
fHead cls = do
  clss <- groupByM' p cls
  let strsss = map (map (\(Clause deps str strs) -> strs)) clss
  return $ map (filter (not . null)) strsss -- *

fHead' :: [[Structure']] -> M [[ClauseTree]]
fHead' [] = return [] -- throwError "should not happen (fHead') 1" -- by semantics of groupByM
fHead' strss = do
  strsss <- groupByM' p'' strss
  mapM f' strsss

p :: Clause' -> Clause' -> M Bool
p (Clause deps str strs) (Clause deps' str' strs') = p' str str'

p' :: Structure' -> Structure' -> M Bool
p' (Struct pnam tms) (Struct pnam' tms') =
  if pnam /= pnam'
    then return False -- throwError "should not happen for well-typed program (p') 1" -- can happen in the call from p'', but not in the call from p
    else do
      mpred <- asks $ find (\(Pred pnam'' _ _ _) -> pnam'' == pnam) . snd
      case mpred of
        Nothing -> throwError "should not happen for well-typed program (p') 2"
        Just (Pred _ _ mo _) -> return $ all (uncurry (==)) [ (tm, tm') | (In, tm, tm') <- zip3 mo tms tms' ]

-- when called, the heads of each str-list (which must all be nonempty) are all exactly equal
f :: [[Structure']] -> M ClauseTree
f [] = throwError "should not happen (f) 1" -- by semantics of groupByM
f strss@((str : _) : _) = do
  let strss' = filter (not . null) $ map tail strss
  strsss <- groupByM' p'' strss'
  ctss <- mapM f' strsss
  return $ CT str ctss
f _ = throwError "should not happen (f) 2" -- by filter (*)

p'' :: [Structure'] -> [Structure'] -> M Bool
p'' (str : strs) (str' : strs') = p' str str'
p'' _ _ = throwError "should not happen (p'')" -- by filter (*)

p''' :: [Structure'] -> [Structure'] -> M Bool
p''' (str : strs) (str' : strs') = return $ str == str'
p''' _ _ = throwError "should not happen (p''')"

-- when called, the heads of each str-list (which must all be nonempty) are all similar
f' :: [[Structure']] -> M [ClauseTree]
f' [] = throwError "should not happen (f')" -- by semantics of groupByM
f' strss = do
  strsss <- groupByM' p''' strss
  mapM f strsss

{-

{T : val}
eval (const T) T.

{B : exp}
eval (neg B) false
  <- foo B true
  <- eval B true
  <- bar B C.

{B : exp}
eval (neg B) true
  <- foo B true
  <- eval B false
  <- baz B D.

---->

[ [] [[CT (foo B true) [[CT (eval B true) [[ CT (bar B C) [[]] ]], CT (eval B false) [[ CT (baz B D) [[]] ]] ]] ]] ]

-}

{- END GEN-TREE -}

getIndex :: M Int
getIndex = do
  i <- get
  modify (+ 1)
  return i

prepareProgram :: Program VarName -> Program'
prepareProgram = mapProgram (Normal . flip (,) 0)

prepareFamilies :: [TypeFam VarName] -> [TypeFam']
prepareFamilies = map (mapTypeFam (Normal . flip (,) 0))

-- all structures are expected to have same predname!
maxDepth :: [Structure'] -> M Int
maxDepth [] = throwError "should not happen (maxDepth) 1"
maxDepth strs@((Struct pnam _) : _) = do
  mpred <- asks $ find (\(Pred pnam' _ _ _) -> pnam' == pnam) . snd
  case mpred of
    Nothing -> throwError "should not happen for well-typed program (maxDepth) 2"
    Just (Pred _ _ mo _) -> return $ maximum (0 : map (maxDepth' mo) strs)
  where
    maxDepth' :: Mode -> Structure' -> Int
    maxDepth' mo (Struct _ tms) = maximum $ 0 : [ termDepth tm | (In, tm) <- zip mo tms ]

substDep :: Term' -> Variable -> Dependency' -> Dependency'
substDep tm x (y, tp)
  | y == x = error $ "oh no" ++ show y ++ show x -- better
  | otherwise = (y, substType tm x tp)

substType :: Term' -> Variable -> Type' -> Type'
substType tm x (Typ tnam tms) = Typ tnam $ map (substTerm tm x) tms

substTerm :: Term' -> Variable -> Term' -> Term'
substTerm tm x (Var y)
  | y == x = tm
  | otherwise = Var y
substTerm tm x (Comp cnam tms) = Comp cnam $ map (substTerm tm x) tms

-- "opposite" of genVars in InputCoverage!
genVars :: Mode -> M [Variable]
genVars [] = return []
genVars (Out : mo) = do
  i <- getIndex
  vars <- genVars mo
  return $ Splitting i 0 : vars
genVars (_ : mo) = do
  i <- getIndex
  vars <- genVars mo
  return $ Normal ("_", i) : vars

genDelta :: [Variable] -> [Dependency'] -> [Dependency']
genDelta [] [] = []
genDelta (x@(Splitting _ _) : xs) ((y, tp) : deps) = (x, tp) : genDelta xs (map (substDep (Var x) y) deps)
genDelta (x@(Normal _) : xs) ((y, tp) : deps) = genDelta xs (map (substDep (Var x) y) deps)
genDelta _ _ = error "should not happen for a mode checked program"

-- all structures are expected to have same predname!
genGoal :: [Structure'] -> M (Structure', [Dependency'])
genGoal [] = throwError "should not happen (genGoal) 1"
genGoal ((Struct pnam _) : _) = do
  mpred <- asks $ find (\(Pred pnam' _ _ _) -> pnam' == pnam) . snd
  case mpred of
    Nothing -> throwError "should not happen for well-typed program (genGoal) 2"
    Just (Pred _ deps mo _) -> do
      args <- genVars mo
      let goal = Struct pnam (map Var args)
      let delta = genDelta args deps
      return (goal, delta)

genConstructors :: Int -> TypeName -> M [(Type', Term', [Dependency'])]
genConstructors depth tnam = do
  mfam <- asks $ find (\(TypeFam tnam' _ _) -> tnam' == tnam) . fst
  case mfam of
    Nothing -> error "should not happen for a well-typed program"
    Just (TypeFam _ _ cns) -> mapM (genConstructor depth) cns

genConstructor :: Int -> Constructor' -> M (Type', Term', [Dependency'])
genConstructor depth (Con cnam deps tp) = do
  args <- mapM (\_ -> getIndex >>= \i -> return (Splitting i (depth + 1))) [1 .. length deps] -- bit hacky
  let object = Comp cnam (map Var args)
  let (delta, tp') = genDelta' args deps tp
  return (tp', object, delta)

-- find another way..
genDelta' :: [Variable] -> [Dependency'] -> Type' -> ([Dependency'], Type')
genDelta' [] [] tp' = ([], tp')
genDelta' (x@(Splitting _ _) : xs) ((y, tp) : deps) tp' =
  let (deps', tp'') = genDelta' xs (map (substDep (Var x) y) deps) tp'
  in ((x, tp) : deps', substType (Var x) y tp'')  
genDelta' (x@(Normal _) : xs) _ _ = error "should not happen genDelta'"
genDelta' _ _ _ = error "should not happen for a mode checked program"

split :: Dependency' -> Structure' -> [Dependency'] -> M [(Structure', [Dependency'])]
split (x@(Splitting _ depth), tp@(Typ tnam _)) goal delta = do
  cns <- genConstructors depth tnam
  return [(goal'', delta''') | (tp', tm, delta') <- cns, Just sub <- [unifyTypes tp tp'],
                               let goal' = substStruct tm x goal, let goal'' = applyStructure sub goal',
                               let delta'' = substDelta tm x (delta ++ delta'), let delta''' = applyDelta sub delta'']
split _ _ _ = error "should not happen, trying to split non-splitting variable"

unifyTypes :: Type' -> Type' -> Maybe Subst'
unifyTypes (Typ tnam tms) (Typ tnam' tms') = unify (Comp tnam tms) (Comp tnam' tms')

substStruct :: Term' -> Variable -> Structure' -> Structure'
substStruct tm x (Struct pnam tms) = Struct pnam $ map (substTerm tm x) tms

substDelta :: Term' -> Variable -> [Dependency'] -> [Dependency']
substDelta tm x [] = []
substDelta tm x ((y, tp) : delta)
  | y == x = substDelta tm x delta
  | otherwise = (y, substType tm x tp) : substDelta tm x delta

applyDelta :: Subst' -> [Dependency'] -> [Dependency']
applyDelta sub [] = []
applyDelta sub ((x, tp) : delta) =
  if sub x == Var x
    then (x, applyType sub tp) : applyDelta sub delta
    else applyDelta sub delta

applyType :: Subst' -> Type' -> Type'
applyType sub (Typ tnam tms) = Typ tnam $ map (applyTerm sub) tms

findSplittingVariable :: Int -> [Dependency'] -> Maybe Dependency'
findSplittingVariable maxDpth delta =
  let
    p (Splitting _ dpth, _) = dpth <= maxDpth
    p _ = False -- shouldn't happen
  in
    case filter p (reverse delta) of
      [] -> Nothing
      (dep : _) -> Just dep

-- temp
outputCoverageCheck :: [TypeFam VarName] -> Program VarName -> Either OutputCoverageError ()
outputCoverageCheck fams prog = runM (outputCoverageCheck' prog') (fams', prog') 0
  where
    fams' = prepareFamilies fams
    prog' = prepareProgram prog

outputCoverageCheck' :: Program' -> M ()
outputCoverageCheck' = mapM_ checkPredicate
  where
    checkPredicate :: Predicate' -> M ()
    checkPredicate pred@(Pred pnam deps mo cls) = do
      -- consider doing a single maxDepth for entire predicate here, i.e. deepest term of all subclauses
      ctsss <- genTree pred
      mapM_ checkTreess ctsss

    checkTreess :: [[ClauseTree]] -> M ()
    checkTreess = mapM_ checkTrees

    checkTrees :: [ClauseTree] -> M ()
    checkTrees cts = checkRoots cts >> mapM_ checkTree cts
    
    checkRoots :: [ClauseTree] -> M ()
    checkRoots cts = do
      let patterns = map (\(CT str _) -> str) cts
      maxDpth <- liftM (1 +) (maxDepth patterns)
      (goal, delta) <- genGoal patterns
      check maxDpth patterns [(goal, delta)]
      put 0
    
    checkTree :: ClauseTree -> M ()
    checkTree = checkTreess . (\(CT _ ctss) -> ctss)

check :: Int -> [Structure'] -> [(Structure', [Dependency'])] -> M ()
check _ _ [] = return ()
check maxDpth heads ((goal, delta) : goals) = do
  if any (\head -> isJust (unifyStructures head (rigidify goal))) heads
    then check maxDpth heads goals
    else
      case findSplittingVariable maxDpth delta of
        Nothing -> throwError $ "output coverage failed: " ++ show (goal, delta) ++ "\n\n\n" ++ show goals
        Just dep -> do
          goals' <- split dep goal delta
          check maxDpth heads $ goals' ++ goals

rigidify :: Structure' -> Structure'
rigidify (Struct pnam tms) = Struct pnam $ map rigidify' tms
  where
    rigidify' :: Term' -> Term'
    rigidify' (Var (Splitting i _)) = Comp ("_" ++ show i) []
    rigidify' tm = tm

unifyStructures :: Structure' -> Structure' -> Maybe Subst'
unifyStructures str1 str2 = unify (structureToTerm str1) (structureToTerm str2)
