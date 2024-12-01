module TermType where

-- за TypeVariable <име на предполагания тип>
-- за TypeFunction <тип на аргумента> <тип на връщаната стойност>
data TermType = TypeVariable String | TypeFunction TermType TermType
  deriving (Eq)

instance Show TermType where
  show (TypeVariable x) = x
  show (TypeFunction x y) = "(" ++ show x ++ ")" ++ " --> " ++ "(" ++ show y ++ ")"
