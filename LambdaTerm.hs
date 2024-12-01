module LambdaTerm where

data LambdaTerm = Variable String | Apply LambdaTerm LambdaTerm | Lambda [String] LambdaTerm
  deriving (Eq)
