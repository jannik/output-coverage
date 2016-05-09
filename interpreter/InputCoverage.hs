module InputCoverage where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State (StateT, evalStateT, get, modify, put)

import Data.List (find)
import Data.Maybe (isJust)

import AST
import PrettyPrinter (ppDependencies, ppStructure)
import Unify (Subst, applyTerm, applyStructure, unify)

data Variable = Normal (VarName, Int) | Splitting Int Int deriving (Eq, Show)

instance PP Variable where
  pp (Normal (vnam, _)) = vnam
  pp (Splitting i _) = "_" ++ show i

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

type InputCoverageError = String
type Environment = [TypeFam']
type Context = Int
type M = ExceptT InputCoverageError (StateT Context (ReaderT Environment Identity))

runM :: M a -> Environment -> Context -> Either InputCoverageError a
runM m env ctx = runIdentity . flip runReaderT env . flip evalStateT ctx . runExceptT $ m

getIndex :: M Int
getIndex = do
  i <- get
  modify (+ 1)
  return i

prepareProgram :: Program VarName -> Program'
prepareProgram = mapProgram (Normal . flip (,) 0)

prepareFamilies :: [TypeFam VarName] -> [TypeFam']
prepareFamilies = map (mapTypeFam (Normal . flip (,) 0))

maxDepth :: Mode -> [Clause'] -> Int
maxDepth mo cls = maximum (0 : map maxDepth' cls)
  where
    maxDepth' :: Clause' -> Int
    maxDepth' (Clause _ (Struct _ tms) _) = maximum $ 0 : [ termDepth tm | (In, tm) <- zip mo tms ]

substDep :: Term' -> Variable -> Dependency' -> Dependency'
substDep tm x (y, tp)
  | y == x = error "should not happen (substDep)"
  | otherwise = (y, substType tm x tp)

substType :: Term' -> Variable -> Type' -> Type'
substType tm x (Typ tnam tms) = Typ tnam $ map (substTerm tm x) tms

substTerm :: Term' -> Variable -> Term' -> Term'
substTerm tm x (Var y)
  | y == x = tm
  | otherwise = Var y
substTerm tm x (Comp cnam tms) = Comp cnam $ map (substTerm tm x) tms

genVars :: Mode -> M [Variable]
genVars [] = return []
genVars (In : mo) = do
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
genDelta _ _ = error "should not happen for a mode checked program (genDelta)"

genGoal :: Predicate' -> M (Structure', [Dependency'])
genGoal (Pred pnam deps mo cls) = do
  args <- genVars mo
  let goal = Struct pnam (map Var args)
  let delta = genDelta args deps
  return (goal, delta)

genConstructors :: Int -> TypeName -> M [(Type', Term', [Dependency'])]
genConstructors depth tnam = do
  mfam <- asks $ find (\(TypeFam tnam' _ _) -> tnam' == tnam)
  case mfam of
    Nothing -> error "should not happen for a well-typed program (genConstructors)"
    Just (TypeFam _ _ cns) -> mapM (genConstructor depth) cns

genConstructor :: Int -> Constructor' -> M (Type', Term', [Dependency'])
genConstructor depth (Con cnam deps tp) = do
  args <- mapM (\_ -> getIndex >>= \i -> return (Splitting i (depth + 1))) [1 .. length deps]
  let object = Comp cnam (map Var args)
  let (delta, tp') = genDelta' args deps tp
  return (tp', object, delta)

genDelta' :: [Variable] -> [Dependency'] -> Type' -> ([Dependency'], Type')
genDelta' [] [] tp' = ([], tp')
genDelta' (x@(Splitting _ _) : xs) ((y, tp) : deps) tp' =
  let (deps', tp'') = genDelta' xs (map (substDep (Var x) y) deps) tp'
  in ((x, tp) : deps', substType (Var x) y tp'')  
genDelta' (x@(Normal _) : xs) _ _ = error "should not happen (genDelta')"
genDelta' _ _ _ = error "should not happen for a mode checked program (genDelta')"

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
    p _ = False -- should not happen
  in
    case filter p (reverse delta) of
      [] -> Nothing
      (dep : _) -> Just dep

inputCoverageCheck :: [TypeFam VarName] -> Program VarName -> Either InputCoverageError ()
inputCoverageCheck fams prog = runM (inputCoverageCheck' $ prepareProgram prog) (prepareFamilies fams) 0

inputCoverageCheck' :: Program' -> M ()
inputCoverageCheck' = mapM_ checkPredicate
  where
    checkPredicate :: Predicate' -> M ()
    checkPredicate pred@(Pred pnam deps mo cls) = do
      let maxDpth = 1 + maxDepth mo cls
          heads = map (\(Clause _ str _) -> str) cls
      (goal, delta) <- genGoal pred
      check maxDpth heads [(goal, delta)]
      put 0

check :: Int -> [Structure'] -> [(Structure', [Dependency'])] -> M ()
check _ _ [] = return ()
check maxDpth heads ((goal, delta) : goals) = do
  if any (\head -> isJust (unifyStructures head (rigidify goal))) heads
    then check maxDpth heads goals
    else
      case findSplittingVariable maxDpth delta of
        Nothing -> throwError $ "uncovered goal >>" ++ ppStructure goal ++ "<< with variables [" ++ ppDependencies delta ++ "]"
        Just dep -> do
          goals' <- split dep goal delta
          check maxDpth heads $ goals' ++ goals

rigidify :: Structure' -> Structure'
rigidify (Struct pnam tms) = Struct pnam $ map rigidify' tms
  where
    rigidify' :: Term' -> Term'
    rigidify' (Var (Splitting i _)) = Comp ("_" ++ show i) []
    rigidify' (Var x) = Var x
    rigidify' (Comp cnam tms) = Comp cnam $ map rigidify' tms

unifyStructures :: Structure' -> Structure' -> Maybe Subst'
unifyStructures str1 str2 = unify (structureToTerm str1) (structureToTerm str2)
