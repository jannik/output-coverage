module Main where

import System.Environment (getArgs)

import Parser
import PrettyPrinter

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
    Right p -> print (Prog p)