module Utils where

import           TermType

type Table = [(String, TermType)]
type TypeInferenceEvaluation = (TermType, Table, Integer)
type Array = [(Int, Int)]
type Transformer typeOfArgument typeOfResult = (typeOfArgument -> typeOfResult)

data Result t = Ok t | Err String

-- съобщение за грешка
cyclicDefinitionErr typeName expr = "Cyclic definition of " ++ typeName ++ " in " ++ expr

-- Обработва Result резултат
-- Ако той е Ок, то обработва стойността(полето на Ok) по начин, диктуван от подадения "трансформатор"
-- Ако той е Err, то връща грешката
unwrap :: Result typeOfArgument -> Transformer typeOfArgument (Result typeOfResult) -> Result typeOfResult
unwrap (Ok t) transformer    = transformer t
unwrap (Err msg) transformer = Err msg

-- Обработва Result резултат
-- Ако той е Ок, то обработва стойността по начин, диктуван от подадения "трансформатор"
-- Ако той е Err, то връща подадената стойност по подразбиране
unwrapNoErr :: Result typeOfArgument -> Transformer typeOfArgument typeOfResult -> typeOfResult -> typeOfResult
unwrapNoErr (Ok t) transformerOk _ = transformerOk t
unwrapNoErr (Err _) _ defaultValue = defaultValue

-- Обработва Result резултат
-- Ако той е Ок, то обработва стойността по начин, диктуван от подадения "трансформатор"
-- Ако той е Err, то връща персонализирана грешка
unwrapCustomErr :: Result typeOfArgument -> Transformer typeOfArgument (Result typeOfResult) -> String -> Result typeOfResult
unwrapCustomErr (Ok t) transformer errorMessage  = transformer t
unwrapCustomErr (Err _) transformer errorMessage = Err errorMessage

instance Show (Result TermType) where
    show (Ok t)    = show t
    show (Err msg) = "Error: " ++ msg


countDigits :: Int -> Int
countDigits n
    | n < 10 = 1
    | otherwise = succ $ countDigits $ quot n 10


-- събира всички числа от имена на типове, които се срещат в даден тип
getAllNumbersInNames :: TermType -> [Int]
getAllNumbersInNames (TypeVariable (_:number)) = [read number]
getAllNumbersInNames (TypeFunction arg body) = getAllNumbersInNames arg ++ getAllNumbersInNames body


quicksort :: [Int] -> [Int]
quicksort []= []
quicksort (el:rest) = quicksort [x | x <- rest, x <= el] ++ [el] ++ quicksort [x | x <- rest, x > el]


-- маха повторения в сортиран масив
unique :: [Int] -> [Int]
unique [] = []
unique [x] = [x]
unique (first:after@(second:rest)) = if first == second then unique after else first : unique after


-- "индексира елементите, правейки "речник"(масив) от типа [<индекс, стойност>]"
enumerate :: [elementType] -> Int -> [(Int, elementType)]
enumerate [] _                = []
enumerate (curr:rest) counter = (counter, curr) : enumerate rest (succ counter)


-- "обхожда дървото на типа в дълбочина и променя имената на типовете с индексите им в масива"
changeAllNames :: TermType -> Array -> TermType
changeAllNames (TypeVariable (_:number)) dict = TypeVariable ("t" ++ show (fst $ head (filter (\(_,old) -> old == read number) dict)))
changeAllNames (TypeFunction arg body) dict = TypeFunction (changeAllNames arg dict) (changeAllNames body dict)


-- for the sake of maintainability and design patterns :)
changeAllNamesFacade :: TermType -> TermType
changeAllNamesFacade termType = changeAllNames termType (enumerate (unique $ quicksort $ getAllNumbersInNames termType) 1)
