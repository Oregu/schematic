module Eval where

import Parse

instance Show LispVal where
    show = showVal

------------- My ShowVal ------------------------
{-
showVal :: LispVal -> String
showVal (Atom str) = "atom " ++ str

showVal (List xs) = "( " ++ (showListVal xs) ++ " )"
showVal (DottedList ls vl) = showListVal ls ++ " . " ++ (showVal vl)

showVal (Number int) = show int
showVal (String s) = "string " ++ s
showVal (Bool b) = show b

showListVal :: [LispVal] -> String
showListVal [] = ""
showListVal [x] = showVal x
showListVal (x:xs) = (showVal x) ++ ',' : ' ' : (showListVal xs)
-}

------------- Book showVal -----------------------

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
