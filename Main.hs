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
    case inferType stack callIndex m of
        Ok (tM, newCallIndexM, _) ->
            case inferType stack newCallIndexM n of
                Ok (tN, newCallIndexN, _) ->
                    let (newReturnType, newCallIndex) = getAvailableVariableTypeName newCallIndexN
                        generatedSubstitutions = generateSubstitutions tM (TypeFunction tN newReturnType)
                            in case generatedSubstitutions of
                                Ok tableWithSubstitutions -> let newType = substitudeTypeWithNewOne tableWithSubstitutions newReturnType
                                    in Ok (newType, newCallIndex, updateStack stack tableWithSubstitutions)
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

-- >>> inferType [] 0 (  Lambda ["x","y"] (Lambda ["a"] (Variable "a")) )
-- [[("a",t2)],[("x",t0),("y",t1)]]

main :: IO ()
main = do
    let expr = "\\xy.xy"
    let term =  Variable "y"
    case inferType [] 0 term of
        Ok (tt, _, _) -> putStrLn $ expr ++ " : " ++ show tt
        Err err       -> putStrLn $ "Error: " ++ err
