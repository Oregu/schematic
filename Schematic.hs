module Schematic where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

import Parse
import Eval

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right val -> val

main :: IO()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
