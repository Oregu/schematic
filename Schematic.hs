module Schematic where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error

import Parse
import Eval
import Error

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

main :: IO()
main = do
	args <- getArgs
	evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
	putStrLn $ extractValue $ trapError evaled
