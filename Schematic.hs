module Schematic where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

import Parse
import Eval

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
