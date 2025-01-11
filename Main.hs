module Main where

import           Inference
import           LambdaTerm
import           Utils


-- Това са тестове, написани по време на разработката
-- ТУК СЕ ПИШАТ ИЗРАЗИ, С КОИТО ДА СЕ ТЕСТВА ПРОГРАМАТА
testCases :: [LambdaTerm]
testCases = [
        Variable "x",
        Apply (Variable "x") (Variable "y"),
        Lambda ["x"] (Variable "x"),
        Lambda ["x", "y"] (Variable "x"),
        Lambda ["x", "y"] (Variable "y"),
        Lambda ["x"] (Apply (Variable "x") (Variable "x")),
        Lambda ["f","x"] (Apply (Variable "f") (Apply (Variable "f") (Variable "x"))),
        Lambda ["x"] (Lambda ["y"] (Apply (Variable "x") (Variable "y"))),
        Lambda ["f","g","x"] (Apply (Variable "f") (Apply (Variable "g") (Variable "x"))),
        Lambda ["a","b"] (Lambda ["x","y"] (Variable "b")),
        Lambda ["x", "y"] (Lambda ["x"] (Variable "x")),
        Lambda ["x", "y", "z"] ( Apply (Lambda ["b"] (Apply (Variable "x") (Variable "y"))) (Lambda [] (Apply (Variable "x") (Variable "z"))) ),
        Lambda ["f","x","y"] (Apply (Apply (Variable "f") (Variable "x")) (Variable "y")),
        Apply (Apply (Lambda ["x"] (Variable "x")) (Lambda ["y"] (Variable "y"))) (Lambda ["z"] (Variable "z"))
    ]

-- минава през изразите като unit tests библиотека(не измислих по-удобно нещо)
runTests :: [(Int, LambdaTerm)] -> Int -> Int -> IO ()
runTests [] _ _ = print "End"
runTests ((num, expression):rest) mx spaces = do
    print $ show num ++ ")" ++ replicate spaces ' ' ++ show expression ++ " : " ++ show (inferTermTypeFacade expression)
    runTests rest mx (mx - countDigits (succ num))

main :: IO ()
main = runTests (enumerate testCases 1) mx (mx-1)
    where mx = succ $ countDigits $ length testCases
