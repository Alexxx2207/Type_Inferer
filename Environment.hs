module Environment where

import           TermType (TermType (..))
import           Utils

type Record = (String, TermType)
type TypeTable = [Record]
type Substitutions = [(TermType,TermType)]
type Stack = [TypeTable]


instance Show (Result (TermType, Integer, Stack)) where
  show (Ok (x, y, z)) = show z
  show (Err msg)      = show msg



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

searchSubstitutions :: TermType -> Substitutions -> Result TermType
searchSubstitutions x [] = Err notFound
searchSubstitutions x (el@(tSearched, subs) : rest)
  | tSearched == x = Ok subs
  | otherwise = searchSubstitutions x rest

-- проверява дали не се съдържа първия аргумент(име на тип) във втория аргумент
-- идеята ми е, че може да се появят циклични оценки на типове
contains :: String -> TermType -> Bool
contains x (TypeVariable name)        = x == name
contains x (TypeFunction arg retType) = contains x arg || contains x retType

-- приема стария и новия тип и връща множеството от полагания
generateSubstitutions :: TermType -> TermType -> Result Substitutions
generateSubstitutions old@(TypeVariable x) new
    | contains x new = Err ("Cyclic type definition " ++ x ++ " in the " ++ show new)
    | otherwise = Ok [(old, new)]
generateSubstitutions (TypeFunction arg1 res1) (TypeFunction arg2 res2) =
    let argumentsSumbstitutions = generateSubstitutions arg1 arg2
        returnTypeSumbstitutions = generateSubstitutions res1 res2
        in case (argumentsSumbstitutions, returnTypeSumbstitutions) of
            (Ok tableForArguments, Ok tableForReturnType) -> Ok (tableForArguments ++ tableForReturnType)
            (Err msg1, Ok _) -> Err msg1
            (Ok _, Err msg2) -> Err msg2
            (Err msg1, Err msg2) -> Err (msg1 ++ " " ++ msg2)


-- приема полагания и стария тип и връща новия тип
substitudeTypeWithNewOne :: Substitutions -> TermType -> TermType
substitudeTypeWithNewOne changes old =
    case searchSubstitutions old changes of
        Ok t  -> t
        Err _ -> old


-- <стара таблица, полаганията> -> нова таблица
updateTypeTable :: TypeTable -> Substitutions -> TypeTable
updateTypeTable oldTable changes = [(name, substitudeTypeWithNewOne changes ty) | (name, ty) <- oldTable]

-- <стар стек, полаганията> -> нова таблица
updateStack :: Stack -> Substitutions -> Stack
updateStack stack changes = map (`updateTypeTable` changes) stack
