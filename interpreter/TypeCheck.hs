module TypeCheck where

import Control.Monad (foldM, guard)
import Data.Either.Utils (maybeToEither)
import Data.List (find, nub, partition)

import AST

type TypingError = String

signatureCheck :: Signature -> Bool
signatureCheck (Sig tps preds) = noNameClashes && conAnnotsOK && predAnnotsOK
  where
    typeNames :: [TypeName]
    typeNames = map (\(Typ tnam _) -> tnam) tps
    
    conNames :: [ConName]
    conNames = [cnam | (Typ _ cns) <- tps, (Con cnam _) <- cns]
    
    predNames :: [PredName]
    predNames = map fst preds
    
    allNames :: [String]
    allNames = typeNames ++ conNames ++ predNames
    
    noNameClashes :: Bool
    noNameClashes = allNames == nub allNames
    
    conAnnotsOK :: Bool
    conAnnotsOK = all (`elem` typeNames) [tp' | (Typ _ cns) <- tps, (Con _ tps') <- cns, tp' <- tps']
    
    predAnnotsOK :: Bool
    predAnnotsOK = all (`elem` typeNames) $ concatMap snd preds

typeCheck :: Signature -> UProgram -> Either TypingError Program
typeCheck (Sig tps preds) uprog = do
  cls <- mapM checkClause uprog
  (cls', preds') <- foldM buildPred (cls, []) preds
  guard $ null cls' -- sanity check; should always be the case
  return preds'
  
  where
    checkClause :: UClause -> Either TypingError Clause
    checkClause (UClause str strs) = checkStructs (str : strs) [] >>= \an ->
      Right $ Clause an (initVars str) (map initVars strs)
    
    checkStructs :: [UStructure] -> Annotation -> Either TypingError Annotation
    checkStructs [] an = Right an
    checkStructs ((UStruct pnam tms) : strs) an = do
      (_, tpSig) <- maybeToEither ("no signature for predicate " ++ pnam) (find (\(pnam', _) -> pnam' == pnam) preds)
      an' <- matchList tms tpSig an
      checkStructs strs an'
    
    matchList :: [UTerm] -> [TypeName] -> Annotation -> Either TypingError Annotation
    matchList [] [] an = Right an
    matchList (tm : tms) (tnam : tpSig) an = match tm tnam an >>= matchList tms tpSig
    matchList tms tpSig _ = Left $ "wrong number of arguments, cannot match " ++ show tms ++ " with " ++ show tpSig
    
    match :: UTerm -> TypeName -> Annotation -> Either TypingError Annotation
    match (UVar vnam) tnam an =
      case lookup vnam an of
        Nothing -> Right $ an ++ [(vnam, tnam)]
        Just tnam'
          | tnam' == tnam -> Right an
          | otherwise -> Left $ "ambiguous type for " ++ vnam ++ ": " ++ tnam' ++ " and " ++ tnam
    match (UComp cnam tms) tnam an = do
      Typ _ cns <- maybeToEither ("no signature for type " ++ tnam) (find (\(Typ tnam' _) -> tnam' == tnam) tps)
      Con _ tpSig <- maybeToEither ("no signature for constructor " ++ cnam ++ " in type " ++ tnam) (find (\(Con cnam' _) -> cnam' == cnam) cns)
      matchList tms tpSig an
    
    buildPred :: ([Clause], [Predicate]) -> (PredName, [TypeName]) -> Either TypingError ([Clause], [Predicate])
    buildPred ([], _) (pnam, _) = Left $ "no clauses for predicate " ++ pnam
    buildPred (cls, preds') (pnam, tps') =
      let (relevantCls, remainingCls) = partition (\(Clause _ (Struct pnam' _) _) -> pnam' == pnam) cls
      in Right (remainingCls, Pred pnam tps' relevantCls : preds')
