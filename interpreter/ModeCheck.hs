module ModeCheck where

import Data.List (find, nub)

import AST

type Variable = VarName

type Program' = Program Variable
type Predicate' = Predicate Variable
type Dependency' = Dependency Variable
type Clause' = Clause Variable
type Structure' = Structure Variable

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

-- todo: add proper error handling
modeCheck :: Program' -> Bool
modeCheck prog = all checkPredicate prog
  where
    checkPredicate :: Predicate' -> Bool
    checkPredicate (Pred _ deps mo cls) = checkDeps deps mo && all checkClause cls
    
    checkDeps :: [Dependency'] -> Mode -> Bool
    checkDeps deps mo =
      let ins = nub [ x | (In, (x, _)) <- zip mo deps ]
      in all (`elem` ins) $ concat $ concat [ map varsTerm tms | (In, (_, Typ _ tms)) <- zip mo deps ]
    
    checkClause :: Clause' -> Bool
    checkClause (Clause _ str strs) = all (`elem` (checkStructs strs (inVars prog str))) (outVars prog str)
    
    checkStructs :: [Structure'] -> [Variable] -> [Variable]
    checkStructs [] grnds = grnds
    checkStructs (str : strs) grnds = checkStructs strs (checkStruct str grnds)
    
    checkStruct :: Structure' -> [Variable] -> [Variable]
    checkStruct str grnds
      | all (`elem` grnds) (inVars prog str) = grnds ++ outVars prog str
      | otherwise = error "mode error (1)"
