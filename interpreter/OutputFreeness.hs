module OutputFreeness where

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
outputFreenessCheck :: Program' -> Bool
outputFreenessCheck prog = all checkPredicate prog
  where
    checkPredicate :: Predicate' -> Bool
    checkPredicate (Pred _ _ _ cls) = all checkClause cls
    
    checkDeps :: [Dependency'] -> Mode -> Bool
    checkDeps deps mo =
      let ins = nub [ x | (In, (x, _)) <- zip mo deps ]
      in all (`elem` ins) $ concat $ concat [ map varsTerm tms | (In, (_, Typ _ tms)) <- zip mo deps ]
    
    checkClause :: Clause' -> Bool
    checkClause (Clause _ str strs) = checkStructs strs $ inVars prog str
    
    checkStructs :: [Structure'] -> [Variable] -> Bool
    checkStructs [] used = True
    checkStructs (str : strs) used =
      case checkStruct str used of
        Nothing -> False
        Just used' -> checkStructs strs used'
    
    -- by mode check, in-vars must have been added earlier and we can ignore them here
    checkStruct :: Structure' -> [Variable] -> Maybe [Variable]
    checkStruct str used = checkStruct' (outVars prog str) used
    
    checkStruct' :: [Variable] -> [Variable] -> Maybe [Variable]
    checkStruct' [] used = Just used
    checkStruct' (x : xs) used =
      if x `elem` used
        then Nothing
        else checkStruct' xs (used ++ [x])
