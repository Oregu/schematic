module Eval where

import Parse

instance Show LispVal where
    show = showVal

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
