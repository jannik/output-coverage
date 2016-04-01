module Main where

import Control.Monad (forM_)

import System.Environment (getArgs)

import AST (Signature (..), varsQuery)
import InputCoverage (inputCoverageCheck)
import ModeCheck (modeCheck)
import Parser (parseFile)
import PrettyPrinter (ppTypes, ppProgram, ppQuery, ppResult)
import Prover (prove)
import TypeCheck (signatureCheck, typeCheck)

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
    Right (sig, uprog, qs)
      | not (signatureCheck sig) -> putStrLn "signature error"
      | otherwise -> do
        case typeCheck sig uprog of
          Left e -> putStrLn e
          Right prog -> do
            let (Sig tps _) = sig
            putStrLn $ ppTypes tps
            putStrLn ""
            putStrLn $ ppProgram prog
            putStrLn "-----"
            forM_ qs $ \q -> do
              putStrLn ""
              putStrLn $ ppQuery q
              putStrLn $ ppResult (varsQuery q) (prove prog q)
            putStrLn "-----"
            putStrLn ""
            if modeCheck prog
              then putStrLn "mode check successful"
              else putStrLn "mode error (2)"
            putStrLn "-----"
            putStrLn ""
            if inputCoverageCheck tps prog
              then putStrLn "input coverage check successful"
              else putStrLn "input coverage error"
            putStrLn "-----"  
