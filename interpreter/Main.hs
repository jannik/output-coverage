module Main where

import Control.Monad (forM_)

import System.Environment (getArgs)

import AST (Signature (..), initVar, varsQuery)
import InputCoverage (inputCoverageCheck)
import ModeCheck (modeCheck)
import Parser (parseFile)
import PrettyPrinter (ppTypeFams, ppProgram, ppQuery, ppResult)
import Prover (prove)
import TypeCheck (buildPredicates, namesOK, typeCheck)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> main' file
    _ -> error "One file name at a time, please."

main' :: FilePath -> IO ()
main' file = do
  res <- parseFile file
  case res of
    Left e -> print e
    Right (sig, cls, qs)
      | not (namesOK sig) -> putStrLn "name error"
      | otherwise -> do
        let prog = buildPredicates cls sig
        let (Sig fams _) = sig
        case typeCheck fams prog of
          Left e -> putStrLn $ "Type error: " ++ e
          Right _ -> do
            putStrLn $ ppTypeFams fams
            putStrLn ""
            putStrLn $ ppProgram prog
            putStrLn "-----"
            forM_ qs $ \q -> do
              putStrLn ""
              putStrLn $ ppQuery q
              putStrLn $ ppResult (map initVar $ varsQuery q) (prove prog q)
            putStrLn "-----"
            putStrLn ""
            if modeCheck prog
              then putStrLn "mode check successful"
              else putStrLn "mode error (2)"
            putStrLn "-----"
            putStrLn ""
            case inputCoverageCheck fams prog of
              Left e -> putStrLn $ "Input coverage error: " ++ e
              Right _ -> do
                putStrLn "input coverage check successful"
                putStrLn "-----"
