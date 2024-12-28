module Inference where

import           Data.Map      as TypeTable
import           LambdaTerm
import           Substitutions
import           TermType
import           Utils

-- генерира тип променлива от нов(произволен) тип
getName :: Integer -> TermType
getName i = TypeVariable ("a" ++ show i)

inferT :: Table -> LambdaTerm -> Integer -> Result (TermType, SubstitutionsTable, Integer)

-- оценява типа на израз, ако е променлива
inferT table (Variable x) i =
    case TypeTable.lookup x table of
        Just t  -> Ok (t, TypeTable.empty, i)
        Nothing -> Err (unboundVariable ++ x)

-- оценява типа на израз, ако е апликация
inferT table (Apply m n) i =
    case inferT table m  i of
        Ok (tM, subsM, iM) ->
            case inferT (appSubstToTable subsM table) n iM of
                Ok (tN, subsN, iN) ->
                    let resTName = getName iN
                    in case genSubst (appSubstToType subsN tM) (TypeFunction tN resTName) of
                        Ok substitutionsApply ->
                            Ok (appSubstToType substitutionsApply resTName, chainSubst substitutionsApply $ chainSubst subsN subsM, succ iN)
                        Err msg -> Err msg
                Err msg -> Err msg
        Err msg -> Err msg

-- оценява типа на израз, ако е абстракция
inferT table (Lambda [] body) i = inferT table body i
inferT table (Lambda (arg:rest) body) i =
    let aType = getName i
    in case inferT (TypeTable.insert arg aType table) (Lambda rest body) (succ i) of
        Ok (t, subst, iNew) -> Ok (TypeFunction (appSubstToType subst aType) t, subst, iNew)
        Err msg -> Err msg

-- фасадна функция за оценяване на тип на израз(за да не пишем в тестовете boilerplate code)
inferTFacade :: LambdaTerm -> Result TermType
inferTFacade term =
    let result = inferT TypeTable.empty term 0
    in case result of
        Ok (t, s, _) -> Ok (appSubstToType s t)
        Err err      -> Err err
