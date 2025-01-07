module LambdaTerm where

-- Variable <име на променливата>
-- Apply <функция> <аргумент>
-- Lambda <[имена на аргументи]> <тяло>
data LambdaTerm = Variable String | Apply LambdaTerm LambdaTerm | Lambda [String] LambdaTerm


-- TODO логика за парсър:
