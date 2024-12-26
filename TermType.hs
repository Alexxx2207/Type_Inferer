module TermType where

-- за TypeVariable <име на предполагания тип>
-- за TypeFunction <тип на аргумента> <тип на връщаната стойност>
data TermType = TypeVariable String | TypeFunction TermType TermType

instance Show TermType where
  show (TypeVariable x)   = x
  show (TypeFunction x y) = "(" ++ show x ++ " --> " ++ show y ++ ")"


instance Eq TermType where
  (==) (TypeVariable x) (TypeVariable y)         = x == y
  (==) (TypeFunction x1 y1) (TypeFunction x2 y2) = x1 == x2 && y1 == y2
  (==) _ _                                       = False
