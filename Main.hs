module Main where

import           Environment
import           LambdaTerm
import           TermType
import           Utils


inferType :: Stack -> Integer -> LambdaTerm -> Result (TermType, Integer)

-- просто (М) променлива
inferType stack callIndex (Variable x) =
    if callIndex == 0
        then let (type_name, _) = getAvailableVariableTypeName 0
            in Ok (type_name, 1)
        else case searchStack x stack of
            Ok (_, tt)   -> Ok (tt, callIndex)
            Err notFound -> Err unboundVariable

-- (MN)
inferType stack callIndex (Apply m n)     = Err ""


-- (λ{args}.M)
inferType stack callIndex (Lambda args m) =
    let (arguments, Ok (targ, callIndexResult)) =
            let (new_table, newCallIndex) = foldl
                    (\(table, currCI) arg ->
                        let (ty, newCI) = getAvailableVariableTypeName currCI
                        in (table ++ [(arg, ty)], newCI)
                    )
                    ([], callIndex)
                    args
                in (new_table, inferType (addNewTable stack new_table) newCallIndex m)
        in Ok (foldr (TypeFunction . snd) targ arguments, callIndexResult)

main :: IO ()
main = do
    let expr = "\\xy.\\x.y"
    let term = Lambda ["x", "y"] (Lambda ["x"] (Variable "x"))
    case inferType [] 0 term of
        Ok (tt, _) -> putStrLn $ expr ++ " : " ++ show tt
        Err err    -> putStrLn $ "Error: " ++ err
