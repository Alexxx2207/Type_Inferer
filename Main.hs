module Main where

import           Inference
import           LambdaTerm
import           Utils


-- Това са тестове, написани по време на разработката
-- ТУК СЕ ПИШАТ ИЗРАЗИ, С КОИТО ДА СЕ ТЕСТВА ПРОГРАМАТА
testCases :: [(Int, LambdaTerm)]
testCases = [
        (1, Variable "x"),
        (2, Apply (Variable "x") (Variable "y")),
        (3, Lambda ["x"] (Variable "x")),
        (4, Lambda ["x", "y"] (Variable "x")),
        (5, Lambda ["x", "y"] (Variable "y")),
        (6, Lambda ["x"] (Apply (Variable "x") (Variable "x"))),
        (7, Lambda ["f","x"] (Apply (Variable "f") (Apply (Variable "f") (Variable "x")))),
        (8, Lambda ["x"] (Lambda ["y"] (Apply (Variable "x") (Variable "y")))),
        (9, Lambda ["f","g","x"] (Apply (Variable "f") (Apply (Variable "g") (Variable "x")))),
        (10, Lambda ["a","b"] (Lambda ["x","y"] (Variable "b"))),
        (11, Lambda ["x", "y"] (Lambda ["x"] (Variable "x"))),
        (12, Lambda ["x", "y", "z"] ( Apply (Lambda ["b"] (Apply (Variable "x") (Variable "y"))) (Lambda [] (Apply (Variable "x") (Variable "z"))) )),
        (13, Lambda ["f","x","y"] (Apply (Apply (Variable "f") (Variable "x")) (Variable "y"))),
        (14, Apply (Apply (Lambda ["x"] (Variable "x")) (Lambda ["y"] (Variable "y"))) (Lambda ["z"] (Variable "z")))
    ]

-- минава през изразите като unit tests библиотека(не измислих по-удобно нещо)
runTests :: [(Int, LambdaTerm)] -> Int -> Int -> IO ()
runTests [] _ _ = print "End"
runTests ((num, expression):rest) mx spaces = do
    print $ show num ++ ")" ++ replicate spaces ' ' ++ show (inferTermTypeFacade expression)
    runTests rest mx (mx - countDigits (succ num))

main :: IO ()
main = runTests testCases mx (mx-1)
    where mx = succ $ countDigits $ length testCases
