module Utils where

import           TermType

data Result t = Ok t | Err String

instance Show (Result TermType) where
    show (Ok t)    = "Type: " ++ show t
    show (Err msg) = "Error: " ++ msg


cyclicDefinition :: String -> TermType -> String
cyclicDefinition inner outer = "Cyclic definition of " ++ inner ++ " in " ++ show outer

unboundVariable = "Unbound variable "

countDigits :: Int -> Int
countDigits n
    | n < 10 = 1
    | otherwise = succ $ countDigits $ quot n 10
