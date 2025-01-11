module LambdaTerm where

-- Variable <име на променливата>
-- Apply <функция> <аргумент>
-- Lambda <[имена на аргументи]> <тяло>
data LambdaTerm = Variable String | Apply LambdaTerm LambdaTerm | Lambda [String] LambdaTerm


instance Show LambdaTerm where
    show (Variable name)           = name
    show (Apply function argument) = "(" ++ show function ++ " " ++ show argument ++ ")"
    show (Lambda names body) = "L" ++ concat names ++ "." ++ show body


-- TODO логика за парсър от String в LambdaTerm:
