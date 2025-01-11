module Substitutions where

import           TableLogic
import           TermType
import           Utils


-- <полагания, стар тип> -> нов тип
-- "сменя" типа поради генерирани за него ограничения
applySubstitutionsToAType :: Table -> TermType -> TermType
-- база(сменя "листата" на типа)
applySubstitutionsToAType substitutions old@(TypeVariable x) =
    unwrapNoErr (searchTable x substitutions) id old

-- търси имената на типове(те са в "листата" на типа)
applySubstitutionsToAType substitutions (TypeFunction arg resType)  =
    TypeFunction (applySubstitutionsToAType substitutions arg) (applySubstitutionsToAType substitutions resType)


-- <стари полагания, нови полагания> -> полагания
-- прилага старите полагания върху новите полагания
-- това е необходима функция, за да може да се приложат полагания върху промеливите от closure или след напасване на типове от тип 'функция'
-- Идеята за тази функционалност е взета(не се сетих за това) от:
-- https://bernsteinbear.com/blog/type-inference/#:~:text=In%20order%20to%20keep%20the%20constraints%20(substitutions)%20flowing%20after%20each%20recursive%20call%20to%20infer_w%2C%20we%20need%20to%20be%20able%20to%20compose%20substitutions.
chainSubst :: Table -> Table -> Table
chainSubst old new = foldr (\(k,v) recRes -> insertPair k v recRes) (changeValues (applySubstitutionsToAType old) new) old


-- генерира полагания така че да си паснат типовете
getSubstitutions :: TermType -> TermType -> Result Table
getSubstitutions (TypeFunction argLeft resLeft)  (TypeFunction argRight resRigbt) =
    unwrap (getSubstitutions resLeft resRigbt)
        (\substFromResults ->
            unwrap (getSubstitutions (applySubstitutionsToAType substFromResults argLeft) (applySubstitutionsToAType substFromResults argRight))
                -- отново, навързването на полагания по този начин не е моя идея
                -- https://bernsteinbear.com/blog/type-inference/#:~:text=compose(%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20unify_w(apply_ty(l%2C%20result)%2C%20apply_ty(r%2C%20result))%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20result%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20)
                (Ok . chainSubst substFromResults)
        )

-- базa:
getSubstitutions left right =
    if left == right
        then Ok []
        else case (left, right) of
            (TypeVariable tName, _) ->
                if contains tName right
                    then Err $ "Cyclic definition of " ++ tName ++ " in " ++ show right
                    else Ok  (insertPair tName right [])
            (_, TypeVariable tName) -> if contains tName left
                then  Err $ "Cyclic definition of " ++ tName ++ " in " ++ show left
                else Ok (insertPair tName left [])



-- проверява дали не се среща тип в типа на израз
-- възможен да се случи сценарии, но невъзможен да се оцени
-- пример: Lambda ["x"] (Apply (Variable "x") (Variable "x")) - иначе типа на х ще е нещо от сорта: т1->т1->т1->т1->т1->...
contains :: String -> TermType -> Bool
contains tName (TypeVariable x)       = x == tName
contains tName (TypeFunction arg res) = contains tName arg || contains tName res
