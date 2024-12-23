module Environment where

import           TermType (TermType (..))
import           Utils

type Record = (String, TermType)
type TypeTable = [Record]
type Stack = [TypeTable]

names = ["t" ++ show x | x <- [0..]]

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

-- проверява дали не се съдържа пъривя аргумент(име на тип) във втория аргумент - идеята ми е, че може да се появят циклични оценки на типове
contains :: String -> TermType -> Bool
contains x (TypeVariable name)        = x == name
contains x (TypeFunction arg retType) = contains x arg || contains x retType

-- приема стария и новия тип и връща множеството от полагания
generateSubstitutions :: TermType -> TermType -> Result TypeTable
generateSubstitutions old new@(TypeVariable x)
    | old == new = Ok []
    | contains x old = Err ("Cyclic type definition " ++ x ++ " in the " ++ show old)
    | otherwise = Ok [(x, old)]
generateSubstitutions old@(TypeVariable x) new
    | old == new = Ok []
    | contains x new = Err ("Cyclic type definition " ++ x ++ " in the " ++ show new)
    | otherwise = Ok [(x, new)]
generateSubstitutions (TypeFunction arg1 res1) (TypeFunction arg2 res2) =
    let argumentsSumbstitutions = generateSubstitutions arg1 arg2
        returnTypeSumbstitutions = generateSubstitutions res1 res2
        in case (argumentsSumbstitutions, returnTypeSumbstitutions) of
            (Ok tableForArguments, Ok tableForReturnType) -> Ok (tableForArguments ++ tableForReturnType)
            (Err msg1, Err msg2) -> Err (msg1 ++ " " ++ msg2)


-- приема полагания и стария тип и връща новия тип
substitudeTypeWithNewOne :: TypeTable -> TermType -> TermType
substitudeTypeWithNewOne changes (TypeVariable x) =
    case searchTable x changes of
        Ok (_,t) -> t
        Err msg  -> TypeVariable x
substitudeTypeWithNewOne changes (TypeFunction arguemntType returnType) =
    TypeFunction (substitudeTypeWithNewOne changes arguemntType) (substitudeTypeWithNewOne changes returnType)


-- <стара таблица, полаганията> -> нова таблица
updateTypeTable :: TypeTable -> TypeTable -> TypeTable
updateTypeTable oldTable changes = [(name, substitudeTypeWithNewOne changes ty) | (name, ty) <- oldTable]
