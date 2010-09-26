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
