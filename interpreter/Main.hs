module Main where

import Control.Monad (forM_)

import System.Environment (getArgs)

import AST (varsQuery)
import Parser (parseFile)
import PrettyPrinter
import Prover (prove)

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
    Right (p, qs) -> do
      print $ Prog p
      putStrLn "-----"
      forM_ qs $ \q -> do
        putStrLn ""
        putStrLn $ ppQuery q
        putStrLn $ ppResult (varsQuery q) (prove p q)
      putStrLn "-----"
