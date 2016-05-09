module TypeCheck where

import Control.Monad.Except (Except, runExcept, throwError)
import Data.List (find, nub)

import AST

type Variable = VarName

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

type TypingError = String

namesOK :: Signature' -> Bool
namesOK (Sig fams preds) = allNames == nub allNames
  where
    allNames = typeNames ++ conNames ++ predNames
    typeNames = [tnam | (TypeFam tnam _ _) <- fams]
    conNames = [cnam | (TypeFam _ _ cns) <- fams, (Con cnam _ _) <- cns]
    predNames = [pnam | (pnam, _, _) <- preds]

-- note: clauses that belong to no predicate are simply discarded and never type checked
buildPredicates :: [Clause'] -> Signature' -> Program'
buildPredicates cls (Sig _ preds) = map (buildPredicate cls) preds

buildPredicate :: [Clause'] -> (PredName, [Dependency'], Mode) -> Predicate'
buildPredicate cls (pnam, deps, mo) = Pred pnam deps mo [cl | cl@(Clause _ (Struct pnam' _) _) <- cls, pnam' == pnam]

typeCheck :: [TypeFam'] -> Program' -> Either TypingError ()
typeCheck fams preds = runExcept $
  checkFamilies [] fams >>
  checkPredicates fams [] preds

checkFamilies :: [TypeFam'] -> [TypeFam'] -> Except TypingError ()
checkFamilies sig [] = return ()
checkFamilies sig (fam : fams) =
  checkFamily sig fam >>
  checkFamilies (sig ++ [fam]) fams

checkFamily :: [TypeFam'] -> TypeFam' -> Except TypingError ()
checkFamily sig (TypeFam tnam deps cns) =
  checkDependencies [] sig deps >>
  mapM_ (checkConstructor [] (sig ++ [TypeFam tnam deps []]) tnam) cns

checkDependencies :: [Dependency'] -> [TypeFam'] -> [Dependency'] -> Except TypingError [Dependency']
checkDependencies env sig [] = return env
checkDependencies env sig ((x, tp) : deps) = do
  kind <- checkType env sig tp
  if null kind
    then return ()
    else throwError "non-primitive type in dependency"
  env' <- updateEnvironment env x tp
  checkDependencies env' sig deps

updateEnvironment :: [Dependency'] -> Variable -> Type' -> Except TypingError [Dependency']
updateEnvironment env x tp =
  case lookup x env of
    Nothing -> return $ env ++ [(x, tp)]
    Just _ -> throwError $ "multiple bindings for variable " ++ pp x

checkType :: [Dependency'] -> [TypeFam'] -> Type' -> Except TypingError [Dependency']
checkType env sig (Typ tnam tms) = checkType' env sig (Typ tnam $ reverse tms)

checkType' :: [Dependency'] -> [TypeFam'] -> Type' -> Except TypingError [Dependency']
checkType' env sig (Typ tnam []) =
  case find (\(TypeFam tnam' _ _) -> tnam' == tnam) sig of
    Nothing -> throwError $ "cannot find type " ++ tnam ++ " in signature"
    Just (TypeFam _ deps _) -> return deps
checkType' env sig (Typ tnam (tm : tms)) = do
  kind <- checkType' env sig (Typ tnam tms)
  case kind of
    [] -> throwError $ "wrong number of arguments for type " ++ tnam
    ((x, tp) : kind') -> do
      tp' <- checkTerm env sig tm
      if tp' == tp
        then return ()
        else throwError $ "type " ++ show tp' ++ " is not " ++ show tp
      return $ substKind tm x kind'

substKind :: Term' -> Variable -> [Dependency'] -> [Dependency']
substKind _ _ [] = []
substKind tm x (dep : deps) = substDep tm x dep : substKind tm x deps

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

checkConstructor :: [Dependency'] -> [TypeFam'] -> TypeName -> Constructor' -> Except TypingError ()
checkConstructor env sig tnam (Con cnam deps tp@(Typ tnam' _))
  | tnam' == tnam = do
      kind <- checkDependencies env sig deps
      kind' <- checkType kind sig tp
      if null kind'
        then return ()
        else throwError "non-primitive type at end of constructor"
  | otherwise = throwError "constructor belonging to wrong family"

checkTerm :: [Dependency'] -> [TypeFam'] -> Term' -> Except TypingError Type'
checkTerm env sig (Var x) =
  case lookup x env of
    Nothing -> throwError $ "free variable " ++ pp x 
    Just tp -> return tp
checkTerm env sig (Comp cnam tms) = do
  (deps, tp) <- checkTerms env sig cnam $ reverse tms
  if null deps
    then return tp
    else throwError $ "not sure, probably type error" ++ show deps

checkTerms :: [Dependency'] -> [TypeFam'] -> ConName -> [Term'] -> Except TypingError ([Dependency'], Type')
checkTerms env sig cnam [] =
  case find (\(Con cnam' _ _) -> cnam' == cnam) (concat [cns | (TypeFam _ _ cns) <- sig]) of
    Nothing -> throwError $ "cannot find constructor " ++ cnam ++ " in signature"
    Just (Con _ deps tp) -> return (deps, tp)
checkTerms env sig cnam (tm : tms) = do
  (deps, tp) <- checkTerms env sig cnam tms
  case deps of
    [] -> throwError $ "wrong number of arguments for constructor " ++ cnam
    ((x, tp') : deps') -> do
      tp'' <- checkTerm env sig tm
      if tp'' == tp'
        then return ()
        else throwError $ "type " ++ show tp'' ++ " is not " ++ show tp'
      return $ (substKind tm x deps', substType tm x tp)
  
checkPredicates :: [TypeFam'] -> [Predicate'] -> [Predicate'] -> Except TypingError ()
checkPredicates sig sig' [] = return ()
checkPredicates sig sig' (pred : preds) =
  checkPredicate sig sig' pred >>
  checkPredicates sig (sig' ++ [pred]) preds

checkPredicate :: [TypeFam'] -> [Predicate'] -> Predicate' -> Except TypingError ()
checkPredicate sig sig' (Pred pnam deps mo cls) =
  checkDependencies [] sig deps >>
  mapM_ (checkClause sig (sig' ++ [Pred pnam deps mo []]) pnam) cls

checkClause :: [TypeFam'] -> [Predicate'] -> PredName -> Clause' -> Except TypingError ()
checkClause sig sig' pnam (Clause deps str@(Struct pnam' _) strs)
  | pnam' == pnam = do
      env <- checkDependencies [] sig deps
      kinds <- mapM (checkStructure env sig sig') (str : strs)
      if all null kinds
        then return ()
        else throwError $ "too few arguments applied to predicate " ++ pnam
  | otherwise = throwError "clause belonging to wrong predicate" -- should not happen

checkStructure :: [Dependency'] -> [TypeFam'] -> [Predicate'] -> Structure' -> Except TypingError [Dependency']
checkStructure env sig sig' (Struct pnam tms) = checkStructure' env sig sig' (Struct pnam $ reverse tms)

checkStructure' :: [Dependency'] -> [TypeFam'] -> [Predicate'] -> Structure' -> Except TypingError [Dependency']
checkStructure' env sig sig' (Struct pnam []) =
  case find (\(Pred pnam' _ _ _) -> pnam' == pnam) sig' of
    Nothing -> throwError $ "undeclared predicate " ++ pnam ++ " in program"
    Just (Pred _ deps _ _) -> return deps
checkStructure' env sig sig' (Struct pnam (tm : tms)) = do
  kind <- checkStructure' env sig sig' (Struct pnam tms)
  case kind of
    [] -> throwError $ "too many arguments applied to predicate " ++ pnam
    ((x, tp) : kind') -> do
      tp' <- checkTerm env sig tm
      if tp' == tp
        then return ()
        else throwError $ "type " ++ show tp' ++ " is not " ++ show tp
      return $ substKind tm x kind'
