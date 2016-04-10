module Main where

import System.IO
import Text.PrettyPrint.HughesPJClass

import Syntax
import Parser

evaluate :: String -> String
evaluate input =
  case parseStat input of
    Left err -> "Syntax error: " ++ show err
    Right expr ->
      show $ pPrint expr


repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  if input == ":q"
    then return ()
    else putStrLn (evaluate input) >> repl
    
main :: IO ()
main = do
  putStrLn "Type :q to quit"
  repl
