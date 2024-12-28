module LambdaTerm where

-- Variable <име на променливата>
-- Apply функция аргумент
-- Lambda [аргументи] тяло
data LambdaTerm = Variable String | Apply LambdaTerm LambdaTerm | Lambda [String] LambdaTerm
