module Inference where

import           Data.Map      as Table
import           LambdaTerm
import           Substitutions
import           TermType
import           Utils

-- генерира нов тип
getName :: Integer -> TermType
getName i = TypeVariable ("a" ++ show i)

inferT :: Table -> LambdaTerm -> Integer -> Result (TermType, Table, Integer)

inferT table (Variable x) i =
    case Table.lookup x table of
        Just t  -> Ok (t, Table.empty, i)
        Nothing -> Err ("Variable " ++ x ++ " not found")

inferT table (Apply m n) i =
    case inferT table m  i of
        Ok (tM, subsM, iM) ->
            case inferT (Table.map (appSubstToType subsM) table) n iM of
                Ok (tN, subsN, iN) ->
                    let resTName = getName iN
                    in case getSubs (appSubstToType subsN tM) (TypeFunction tN resTName) of
                        Ok substitutionsApply ->
                            -- отново необходимостта за композиция на полагания я взех от:
                            -- https://bernsteinbear.com/blog/type-inference/#:~:text=return%20compose(s3%2C%20compose(s2%2C%20s1))%2C%20apply_ty(r%2C%20s3)
                            let finalSubs = chainSubst substitutionsApply $ chainSubst subsN subsM
                            in Ok (appSubstToType finalSubs resTName, finalSubs, succ iN)
                        Err msg -> Err msg
                Err msg -> Err msg
        Err msg -> Err msg

inferT table (Lambda [] body) i = inferT table body i
inferT table (Lambda (arg:rest) body) i =
    let aType = getName i
    in case inferT (Table.insert arg aType table) (Lambda rest body) (succ i) of
        Ok (t, subst, iNew) -> Ok (appSubstToType subst (TypeFunction (appSubstToType subst aType) t), subst, iNew)
        Err msg -> Err msg

-- фасадна функция за оценяване на тип на израз(за да не пишем в тестовете boilerplate code)
inferTFacade :: LambdaTerm -> Result TermType
inferTFacade term =
    let result = inferT Table.empty term 0
    in case result of
        Ok (t, _, _) -> Ok t
        Err err          -> Err err
