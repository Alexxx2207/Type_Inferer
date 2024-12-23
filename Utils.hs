module Utils where

data Result t = Ok t | Err String

{-  Err constants
    Not found = променливата не е намерена в таблицата с типове
    Unbound variable = променливата не е дефинирана чрез абстракция
-}

notFound = "Not found"
unboundVariable = "Unbound variable"
