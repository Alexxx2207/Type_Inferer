module Utils where

import           TermType

showEither :: Either TermType String -> String
showEither (Left t)    = "Type: " ++ show t
showEither (Right err) = "Error: " ++ err


cyclicDefinition :: String -> TermType -> String
cyclicDefinition inner outer = "Cyclic definition of " ++ inner ++ " in " ++ show outer

unboundVariable = "Unbound variable "

countDigits :: Int -> Int
countDigits n
    | n < 10 = 1
    | otherwise = succ $ countDigits $ quot n 10
