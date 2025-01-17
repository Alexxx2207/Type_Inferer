module Inference where

import           LambdaTerm
import           Substitutions
import           TableLogic
import           TermType
import           Utils

-- Оценява типа на израз на база израза, типовете на "видимите" променливи и на броя вече създадени променливи
-- <израз за оценяване, таблица с променливи и техните типове, брой създадени вече променливи> -> оценка на израза
inferTermType :: LambdaTerm -> Table -> Integer -> Result TypeInferenceEvaluation

-- база
-- просто изразът променлива: "х"
inferTermType (Variable x) table variablesCounter =
    unwrapCustomErr (searchTable x table) (\t -> Ok (t, [], variablesCounter)) ("Variable " ++ x ++ " not found")

-- (MN)
inferTermType (Apply m n) table variablesCounter =
    unwrap (inferTermType m table variablesCounter)
        (\(typeM, substitutionsM, variablesCounterM) ->
            unwrap (inferTermType n (changeValues (applySubstitutionsToAType substitutionsM) table) variablesCounterM)
                (\(typeN, substitutionsN, variablesCounterN) ->
                    let resTName = TypeVariable ("t" ++ show variablesCounterN)
                        chainedSubstitutionsFromChildrenNodes = chainSubst substitutionsN substitutionsM
                    in unwrap (getSubstitutions (applySubstitutionsToAType chainedSubstitutionsFromChildrenNodes typeM) (TypeFunction (applySubstitutionsToAType chainedSubstitutionsFromChildrenNodes typeN) resTName))
                        (\substitutionsApply ->
                            -- отново, начина за навързване на полаганията я взех от:
                            -- https://bernsteinbear.com/blog/type-inference/#:~:text=return%20compose(s3%2C%20compose(s2%2C%20s1))%2C%20apply_ty(r%2C%20s3)
                            let finalSubs = chainSubst substitutionsApply chainedSubstitutionsFromChildrenNodes
                            in Ok (applySubstitutionsToAType finalSubs resTName, finalSubs, succ variablesCounterN)
                        )
                )
        )

-- база на "разгръщането" на аргументите на ламбда функция
inferTermType (Lambda [] body) table variablesCounter = inferTermType body table variablesCounter

-- λ{<arg>}.<body>
inferTermType (Lambda (arg:rest) body) table variablesCounter =
    let argType = TypeVariable ("t" ++ show variablesCounter)
    in unwrap (inferTermType (Lambda rest body) (insertPair arg argType table) (succ variablesCounter))
        (\(bodyType, substitutions, variablesCounterNew) ->
            -- прилагане на полаганията върху аргументите, които аргументи са бил дошли от closure при оценяване на тялото
            Ok (TypeFunction  (applySubstitutionsToAType substitutions  argType) bodyType, substitutions, variablesCounterNew)
        )

-- фасадна функция за оценяване на тип на израз(за да не повтаряме в тестовете код)
inferTermTypeFacade :: LambdaTerm -> Result TermType
inferTermTypeFacade term = unwrap (inferTermType term [] 1) (\(t, _, _) -> Ok (changeAllNamesFacade t))
