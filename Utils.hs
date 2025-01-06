module Utils where

import           TermType

data Result t = Ok t | Err String

instance Show (Result TermType) where
    show (Ok t)    = "Type: " ++ show t
    show (Err msg) = "Error: " ++ msg


countDigits :: Int -> Int
countDigits n
    | n < 10 = 1
    | otherwise = succ $ countDigits $ quot n 10


type Array = [(Int, Int)];

-- събира всички числа от имена на типове, които се срещат в даден тип
getAllNumbersInNames :: TermType -> [Int]
getAllNumbersInNames (TypeVariable (_:number))        = [read number]
getAllNumbersInNames (TypeFunction arg body) = getAllNumbersInNames arg ++ getAllNumbersInNames body


quicksort :: [Int] -> [Int]
quicksort []        = []
quicksort (el:rest) = quicksort [x | x <- rest, x <= el] ++ [el] ++ quicksort [x | x <- rest, x > el]


-- маха повторения в сортиран масив
unique :: [Int] -> [Int]
unique [] = []
unique [x] = [x]
unique (first:after@(second:rest)) = if first == second then unique after else first : unique after


-- "индексира елементите, правейки "речник"(масив) от типа [<индекс, стойност>]"
enumerateNumbers :: [Int] -> Int -> Array
enumerateNumbers [] _ = []
enumerateNumbers (curr:rest) counter = (counter, curr) : enumerateNumbers rest (succ counter)


-- "обхожда дървото на типа в дълбочина и променя имената на типовете с индексите им в масива"
changeAllNames :: TermType -> Array -> TermType
changeAllNames (TypeVariable (_:number)) dict = TypeVariable ("x" ++ show (fst $ head (filter (\(_,old) -> old == read number) dict)))
changeAllNames (TypeFunction arg body) dict = TypeFunction (changeAllNames arg dict) (changeAllNames body dict)


-- for the sake of maintainability and design patterns :)
changeAllNamesFacade :: TermType -> TermType
changeAllNamesFacade termType = changeAllNames termType (enumerateNumbers (unique $ quicksort $ getAllNumbersInNames termType) 1)
