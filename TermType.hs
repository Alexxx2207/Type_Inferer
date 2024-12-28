module TermType where

-- TypeVariable <име на предполагания тип>
-- TypeFunction <тип на аргумента> <тип на връщаната стойност>
data TermType = TypeVariable String | TypeFunction TermType TermType

instance Show TermType where
    show (TypeVariable s)   = s
    show (TypeFunction a b) =
        case a of
            TypeFunction _ _ -> "(" ++ show a ++ ") -> " ++ show b
            _                -> show a ++ " -> " ++ show b

instance Eq TermType where
    (==) (TypeVariable x) (TypeVariable y)         = x == y
    (==) (TypeFunction x1 y1) (TypeFunction x2 y2) = x1 == x2 && y1 == y2
    (==) _ _                                       = False
