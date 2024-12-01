module Environment where

import           TermType (TermType (..))
import           Utils

-- (име, променлива|функция , типа е окончателен , връщан тип , приемащ тип))
type Record = (String, TermType)
type TypeTable = [Record]
type Stack = [TypeTable]

names = ["t" ++ show x | x <- [0..]]

containsType :: String -> TermType -> Bool
containsType t (TypeVariable tv)    = t == tv
containsType t (TypeFunction t1 t2) = containsType t t1 || containsType t t2

getAvailableVariableTypeName :: Integer -> (TermType, Integer)
getAvailableVariableTypeName callIndex = (TypeVariable ("t" ++ show callIndex), succ callIndex)

searchTable :: String -> TypeTable -> Result Record
searchTable x [] = Err notFound
searchTable x (el@(name, _) : rest)
  | name == x = Ok el
  | otherwise = searchTable x rest

searchStack :: String -> Stack -> Result Record
searchStack x [] = Err notFound
searchStack x (typeTable : rest) =
    case searchTable x typeTable of
        Ok record    -> Ok record
        Err notFound -> searchStack x rest

addNewTable :: [[Record]] -> [Record] -> [[Record]]
addNewTable stack table = table : stack

removeTable :: [[Record]] -> [[Record]]
removeTable []    = []
removeTable stack = tail stack
