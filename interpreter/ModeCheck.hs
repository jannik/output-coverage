module ModeCheck where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State (StateT, evalStateT, get, modify, put)

import Data.List (find, nub)

import AST
import Unify (Subst, applyTerm, applyStructure, unify)

type Variable = (VarName, Int)

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

type ModeError = String
type Environment = [TypeFam']
type Context = Int
type M = ExceptT ModeError (StateT Context (ReaderT Environment Identity))

data ModeResult = ModeNormal [Variable] | ModeAbsurd deriving (Eq, Show)

-- begin {stolen library functions}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p [] = return False
anyM p (x:xs) = ifM (p x) (return True) (anyM p xs)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p [] = return True
allM p (x:xs) = ifM (p x) (allM p xs) (return False)
-- end {stolen library functions}

runM :: M a -> Environment -> Context -> Either ModeError a
runM m env ctx = runIdentity . flip runReaderT env . flip evalStateT ctx . runExceptT $ m

getIndex :: M Int
getIndex = do
  i <- get
  modify (+ 1)
  return i

prepareProgram :: Program VarName -> Program'
prepareProgram = mapProgram (flip (,) 0)

prepareFamilies :: [TypeFam VarName] -> [TypeFam']
prepareFamilies = map (mapTypeFam (flip (,) 0))

-- unsafe
inVars :: Program' -> Structure' -> [Variable]
inVars prog (Struct pnam tms) =
  case find (\(Pred pnam' _ _ _) -> pnam' == pnam) prog of
    Nothing -> error "should not happen for type checked programs (1)"
    Just (Pred _ _ mo _)
      | length mo /= length tms -> error "should not happen for type checked programs (2)"
      | otherwise -> nub $ concat [ varsTerm tm | (In, tm) <- zip mo tms ]

-- unsafe
outVars :: Program' -> Structure' -> [Variable]
outVars prog (Struct pnam tms) =
  case find (\(Pred pnam' _ _ _) -> pnam' == pnam) prog of
    Nothing -> error "should not happen for type checked programs (3)"
    Just (Pred _ _ mo _)
      | length mo /= length tms -> error "should not happen for type checked programs (4)"
      | otherwise -> nub $ concat [ varsTerm tm | (Out, tm) <- zip mo tms ]

modeCheck :: [TypeFam VarName] -> Program VarName -> Either ModeError ()
modeCheck fams prog = runM (modeCheck' $ prepareProgram prog) (prepareFamilies fams) 0

modeCheck' :: Program' -> M ()
modeCheck' prog = mapM_ checkPredicate prog
  where
    checkPredicate :: Predicate' -> M ()
    checkPredicate (Pred pnam deps mo cls) =
      if checkDeps deps mo
        then mapM_ checkClause cls
        else throwError $ "dependencies not mode correct for predicate " ++ pnam
    
    checkDeps :: [Dependency'] -> Mode -> Bool
    checkDeps deps mo =
      let ins = nub [ x | (In, (x, _)) <- zip mo deps ]
      in all (`elem` ins) $ concat $ concat [ map varsTerm tms | (In, (_, Typ _ tms)) <- zip mo deps ]
    
    checkClause :: Clause' -> M ()
    checkClause (Clause deps str strs) = do
      res <- checkStructs strs (inVars prog str) deps
      case res of
        ModeNormal grnds ->
          if all (`elem` grnds) (outVars prog str)
            then return ()
            else throwError "head has non-ground vars in output (checkClause)"
        ModeAbsurd -> return ()
    
    checkStructs :: [Structure'] -> [Variable] -> [Dependency'] -> M ModeResult
    checkStructs [] grnds deps = return $ ModeNormal grnds
    checkStructs (str : strs) grnds deps = do
      res <- checkStruct str grnds deps
      case res of
        ModeNormal grnds' -> checkStructs strs grnds' deps
        ModeAbsurd -> return $ ModeAbsurd
    
    checkStruct :: Structure' -> [Variable] -> [Dependency'] -> M ModeResult
    checkStruct str grnds deps
      | all (`elem` grnds) (inVars prog str) = do
          b <- anyM (emptyType 0 100 [] deps) (outVars prog str) -- 100 is hardcoded maxdepth, for logging uses only
          put 0
          if b
            then return $ ModeAbsurd
            else return $ ModeNormal $ grnds ++ outVars prog str
      | otherwise = throwError "non-ground vars in output (checkStruct)"

emptyType :: Int -> Int -> [Type'] -> [Dependency'] -> Variable -> M Bool
emptyType dpth maxDpth tps deps x
  | dpth > maxDpth = throwError $ "max depth exceeded (emptyType) for variable " ++ show x -- temp
  | otherwise =
    case lookup x deps of
      Nothing -> throwError "should not happen for a well-typed program (emptyType)"
      Just tp -> emptyType' dpth maxDpth (tps ++ [tp]) tp

emptyType' :: Int -> Int -> [Type'] -> Type' -> M Bool
emptyType' dpth maxDpth tps tp@(Typ tnam _) = do
  cns <- genConstructors tnam
  let depss = [deps' | (tp', _, deps) <- cns, Just sub <- [unifyTypes tp tp'],
                       let deps' = applyDelta sub deps]
  allM (emptyType'' dpth maxDpth tps) depss

emptyType'' :: Int -> Int -> [Type'] -> [Dependency'] -> M Bool
emptyType'' dpth maxDpth tps deps =
  if any (`elem` (map (\(Typ tnam _) -> tnam) tps)) (map (\(_, Typ tnam _) -> tnam) deps)
    then return False -- give up due to recursion
    else anyM (emptyType dpth maxDpth tps deps . fst) deps

-- quick solutions borrowed from inputcoverage module:

genConstructors :: TypeName -> M [(Type', Term', [Dependency'])]
genConstructors tnam = do
  mfam <- asks $ find (\(TypeFam tnam' _ _) -> tnam' == tnam)
  case mfam of
    Nothing -> error "should not happen for a well-typed program (genConstructors)"
    Just (TypeFam _ _ cns) -> mapM genConstructor cns

genConstructor :: Constructor' -> M (Type', Term', [Dependency'])
genConstructor (Con cnam deps tp) = do
  args <- mapM (\_ -> getIndex >>= \i -> return ("_", i)) [1 .. length deps]
  let object = Comp cnam (map Var args)
  let (delta, tp') = genDelta' args deps tp
  return (tp', object, delta)

genDelta' :: [Variable] -> [Dependency'] -> Type' -> ([Dependency'], Type')
genDelta' [] [] tp' = ([], tp')
genDelta' (x : xs) ((y, tp) : deps) tp' =
  let (deps', tp'') = genDelta' xs (map (substDep (Var x) y) deps) tp'
  in ((x, tp) : deps', substType (Var x) y tp'')
genDelta' _ _ _ = error "should not happen for a mode checked program (genDelta')"

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

unifyTypes :: Type' -> Type' -> Maybe Subst'
unifyTypes (Typ tnam tms) (Typ tnam' tms') = unify (Comp tnam tms) (Comp tnam' tms')

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
