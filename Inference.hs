module Inference where

import           Data.Map    as TypeTable
import           Environment
import           LambdaTerm
import           TermType
import           Utils

-- генерира тип променлива от нов(произволен) тип
getName :: Integer -> TermType
getName i = TypeVariable ("a" ++ show i)

inferT :: Stack -> LambdaTerm -> Integer -> Result (TermType, Substitutions, Integer)

-- оценява типа на израз, ако е променлива
inferT st (Variable x) i =
    case TypeTable.lookup x st of
        Just t  -> Ok (t, TypeTable.empty, i)
        Nothing -> Err (unboundVariable ++ x)

-- оценява типа на израз, ако е апликация
inferT st (Apply m n) i =
    case inferT st m  i of
        Ok (tM, subsM, iM) ->
            case inferT (appSubstToStack subsM st) n iM of
                Ok (tN, subsN, iN) ->
                    let resTName = getName iN
                    in case genSubst (appSubstToType subsN tM) (TypeFunction tN resTName) of
                        Ok substitutionsApply ->
                            Ok (appSubstToType substitutionsApply resTName, chainSubst substitutionsApply $ chainSubst subsN subsM, succ iN)
                        Err msg -> Err msg
                Err msg -> Err msg
        Err msg -> Err msg

-- оценява типа на израз, ако е абстракция
inferT st (Lambda [] body) i = inferT st body i
inferT st (Lambda (arg:rest) body) i =
    let aType = getName i
    in case inferT (TypeTable.insert arg aType st) (Lambda rest body) (succ i) of
        Ok (t, subst, iNew) -> Ok (TypeFunction (appSubstToType subst aType) t, subst, iNew)
        Err msg -> Err msg

-- фасадна функция за оценяване на тип на израз(за да не пишем в тестовете boilerplate code)
inferTFacade :: LambdaTerm -> Result TermType
inferTFacade term =
    let result = inferT TypeTable.empty term 0
    in case result of
        Ok (t, s, _) -> Ok (appSubstToType s t)
        Err err      -> Err err
