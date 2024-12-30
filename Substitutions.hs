module Substitutions where

import           Data.Map as Table
import           TermType
import           Utils


type Table = Table.Map String TermType

-- <полагания, стар тип> -> нов тип
appSubstToType :: Table -> TermType -> TermType
appSubstToType substitutions old@(TypeVariable x) =
    case Table.lookup x substitutions of
        Just val -> val
        Nothing  -> old

appSubstToType substitutions (TypeFunction arg1 res1)  = TypeFunction (appSubstToType substitutions arg1) (appSubstToType substitutions res1)

-- <стари полагания, нови полагания> -> полагания
-- прилага старите полагания върху новите полагания
-- това е необходима функция, за да може да се приложат полагания върху промеливите от closure или след напасване на типове от тип 'функция'
-- Идеята за тази функционалност е взета от:
-- https://bernsteinbear.com/blog/type-inference/#:~:text=In%20order%20to%20keep%20the%20constraints%20(substitutions)%20flowing%20after%20each%20recursive%20call%20to%20infer_w%2C%20we%20need%20to%20be%20able%20to%20compose%20substitutions.
chainSubst :: Table -> Table -> Table
chainSubst old new = Prelude.foldr
    (\k recRes ->
        case Table.lookup k old of
            Just  value -> Table.insert k value recRes
            Nothing     -> recRes
    )
    (Table.map (appSubstToType old) new)
    (Table.keys old)


-- генерира полагания така че да си паснат типовете
getSubs :: TermType -> TermType -> Result Table
getSubs (TypeFunction arg1 res1)  (TypeFunction arg2 res2) =
    case getSubs arg1 arg2 of
        Ok argSubst ->
            case getSubs (appSubstToType argSubst res1) (appSubstToType argSubst res2) of
                Ok retSubst -> Ok $ chainSubst retSubst argSubst
                Err msg     -> Err msg
        Err msg -> Err msg

-- бази:

getSubs old@(TypeVariable tName)  target
    | target == old = Ok Table.empty
    | contains tName target = Err $ "Cyclic definition of " ++ tName ++ " in " ++ show target
    | otherwise = Ok  (Table.insert tName target Table.empty)

-- за случая (а->б)->с и a->c - примерно:
getSubs target old@(TypeVariable tName)
    | target == old = Ok Table.empty
    | contains tName target = Err $ "Cyclic definition of " ++ tName ++ " in " ++ show target
    | otherwise = Ok (Table.insert tName target Table.empty)


-- проверява дали не се среща тип в типа на израз
contains :: String -> TermType -> Bool
contains tName (TypeVariable x) = x == tName
contains tName (TypeFunction arg1 res1) = contains tName arg1 || contains tName res1
