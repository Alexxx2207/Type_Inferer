module Substitutions where

import           TableLogic
import           TermType
import           Utils


-- <полагания, стар тип> -> нов тип
-- "сменя" типа поради генерирани за него ограничения
applySubstitutionsToAType :: Table -> TermType -> TermType
applySubstitutionsToAType substitutions old@(TypeVariable x) =
    case searchTable x substitutions of
        Ok v  -> v
        Err _ -> old

applySubstitutionsToAType substitutions (TypeFunction arg resType)  =
    TypeFunction (applySubstitutionsToAType substitutions arg) (applySubstitutionsToAType substitutions resType)


-- <стари полагания, нови полагания> -> полагания
-- прилага старите полагания върху новите полагания
-- това е необходима функция, за да може да се приложат полагания върху промеливите от closure или след напасване на типове от тип 'функция'
-- Идеята за тази функционалност е взета от:
-- https://bernsteinbear.com/blog/type-inference/#:~:text=In%20order%20to%20keep%20the%20constraints%20(substitutions)%20flowing%20after%20each%20recursive%20call%20to%20infer_w%2C%20we%20need%20to%20be%20able%20to%20compose%20substitutions.
chainSubst :: Table -> Table -> Table
chainSubst old new = foldr (\(k,v) recRes -> insertPair k v recRes) (changeValues (applySubstitutionsToAType old) new) old


-- генерира полагания така че да си паснат типовете
getSubs :: TermType -> TermType -> Result Table
getSubs (TypeFunction arg1 res1)  (TypeFunction arg2 res2) =
    case getSubs arg1 arg2 of
        Ok argSubst ->
            case getSubs (applySubstitutionsToAType argSubst res1) (applySubstitutionsToAType argSubst res2) of
                -- отново, навързването на полагания по този начин не е моя идея
                -- https://bernsteinbear.com/blog/type-inference/#:~:text=compose(%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20unify_w(apply_ty(l%2C%20result)%2C%20apply_ty(r%2C%20result))%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20result%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20)
                Ok retSubst -> Ok $ chainSubst retSubst argSubst
                Err msg     -> Err msg
        Err msg -> Err msg

-- бази:
-- не е сигурно кой(левия или десния) тип е "по-общ"

-- примерно за случая а и (б->в)->г:
getSubs old@(TypeVariable tName)  target
    | target == old = Ok []
    | contains tName target = Err $ "Cyclic definition of " ++ tName ++ " in " ++ show target
    | otherwise = Ok  (insertPair tName target [])

-- примерно за случая (а->б)->в и г->д:
getSubs target old@(TypeVariable tName)
    | target == old = Ok []
    | contains tName target = Err $ "Cyclic definition of " ++ tName ++ " in " ++ show target
    | otherwise = Ok (insertPair tName target [])


-- проверява дали не се среща тип в типа на израз
-- възможен да се случи сценарии, но невъзможен да се оцени
-- пример: Lambda ["x"] (Apply (Variable "x") (Variable "x")) - иначе типа на х ще е нещо от сорта: т1->т1->т1->т1->т1->...
contains :: String -> TermType -> Bool
contains tName (TypeVariable x)       = x == tName
contains tName (TypeFunction arg res) = contains tName arg || contains tName res
