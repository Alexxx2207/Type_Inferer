module Inference where

import           LambdaTerm
import           Substitutions
import           TableLogic
import           TermType
import           Utils

-- за яснота
-- <тип на израза, таблица с полагания, брой създадени вече променливи>
type TypeInferenceEvaluation = (TermType, Table, Integer)

-- Оценява типа на израз на база израза, типовете на "видимите" променливи и на броя вече създадени променливи
-- <израз за оценяване, таблица с променливи и техните типове, брой създадени вече променливи> -> оценка на израза
inferTermType :: LambdaTerm -> Table -> Integer -> Result TypeInferenceEvaluation

-- база
-- просто изразът променлива: "х"
inferTermType (Variable x) table i = case searchTable x table of
        Ok t  -> Ok (t, [], i)
        Err _ -> Err ("Variable " ++ x ++ " not found")

-- (MN)
inferTermType (Apply m n) table i = case inferTermType m table i of
    Ok (tM, subsM, iM) -> case inferTermType n (changeValues (applySubstitutionsToAType subsM) table) iM of
        Ok (tN, subsN, iN) ->
            let resTName = TypeVariable ("x" ++ show iN)
            in case getSubs (applySubstitutionsToAType subsN tM) (TypeFunction tN resTName) of
                Ok substitutionsApply ->
                    -- отново, начина за композиция на полаганията я взех от:
                    -- https://bernsteinbear.com/blog/type-inference/#:~:text=return%20compose(s3%2C%20compose(s2%2C%20s1))%2C%20apply_ty(r%2C%20s3)
                    let finalSubs = chainSubst substitutionsApply $ chainSubst subsN subsM
                    in Ok (applySubstitutionsToAType finalSubs resTName, finalSubs, succ iN)
                Err msg -> Err msg
        Err msg -> Err msg
    Err msg -> Err msg

-- база на "разгръщането" на аргументите на ламбда функция
inferTermType (Lambda [] body) table i = inferTermType body table i

-- λ{<arg>}.<body>
inferTermType (Lambda (arg:rest) body) table i =
    let argType = TypeVariable ("x" ++ show i)
    in case inferTermType (Lambda rest body) (insertPair arg argType table) (succ i) of
        Ok (t, subst, iNew) -> Ok (TypeFunction (applySubstitutionsToAType subst argType) t, subst, iNew)
        Err msg -> Err msg

-- фасадна функция за оценяване на тип на израз(за да не повтаряме в тестовете код)
inferTermTypeFacade :: LambdaTerm -> Result TermType
inferTermTypeFacade term = case inferTermType term [] 1 of
        Ok (t, _, _) -> Ok (changeAllNamesFacade t)
        Err err      -> Err err
