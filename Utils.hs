module Utils where

data Result t = Ok t | Err String

{-  Err constants
    Not found = променливата не е намерена в таблицата с типове
-}

notFound = "Not found"
unboundVariable = "unboundVariable"
