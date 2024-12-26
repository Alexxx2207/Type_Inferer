module Main where

import           Control.Monad.State
import           Environment
import           LambdaTerm
import           TermType
import           Utils



inferType :: Stack -> Integer -> LambdaTerm -> Result (TermType, Integer, Stack)

-- просто (<М>) променлива
inferType stack callIndex (Variable x) =
    if callIndex == 0
        then let (type_name, _) = getAvailableVariableTypeName 0
            in Ok (type_name, 1, stack)
        else case searchStack x stack of
            Ok (_, tt)   -> Ok (tt, callIndex, stack)
            Err notFound -> Err unboundVariable

-- (<M><N>)
inferType stack callIndex (Apply m n) =
    case inferType stack callIndex n of
        Ok (tN, newCallIndexN, modified_stackN) ->
            case inferType modified_stackN newCallIndexN m of
                Ok (tM, newCallIndexM, modified_stackM) ->
                    let (newReturnType, newCallIndex) = getAvailableVariableTypeName newCallIndexM
                        generatedSubstitutions = generateSubstitutions tM (TypeFunction tN newReturnType)
                            in case generatedSubstitutions of
                                Ok tableWithSubstitutions ->
                                    let newType = substitudeTypeWithNewOne tableWithSubstitutions newReturnType
                                        in Ok (newType, newCallIndex, updateStack modified_stackM tableWithSubstitutions)
                                Err msg -> Err msg
                Err msg -> Err msg
        Err msg -> Err msg




-- (λ{<arg> {,<arg>}}.<M>)
inferType stack callIndex (Lambda args m) =
    let (arguments, result) =
            let (new_table, newCallIndex) = foldl
                    (\(table, currCI) arg ->
                        let (ty, newCI) = getAvailableVariableTypeName currCI
                        in (table ++ [(arg, ty)], newCI)
                    )
                    ([], callIndex)
                    args
                in (new_table, inferType (new_table : stack) newCallIndex m)
        in case result of
            Ok (targ, callIndexResult, modified_stack) -> Ok (foldr (TypeFunction . snd) targ (head modified_stack), callIndexResult, tail modified_stack)
            Err msg -> Err msg

main :: IO ()
main = do
    let term =   Lambda ["x","y"] ( Lambda ["a","b"] ( Apply (Variable "x") (Apply (Variable "a") (Variable "y")) ) )
    case inferType [] 0 term of
        Ok (tt, _, _) -> print tt
        Err err       -> putStrLn $ "Error: " ++ err
