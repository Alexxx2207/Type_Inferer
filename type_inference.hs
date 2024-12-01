module Project where

data Type = Variable String | Function Type Type
    deriving (Eq)

instance Show Type where
    show (Variable x) = x
    show (Function x y) = "(" ++ show x ++ ")" ++ " --> " ++ "(" ++ show y ++ ")"

data LambdaTerm = Argument String | Apply LambdaTerm LambdaTerm | Lambda String LambdaTerm
    deriving (Eq, Show)

type TypeTable = [(String, Type)]

greekAlphabet = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω", "Α", "Β", "Γ", "Δ", "Ε", "Ζ", "Η", "Θ", "Ι", "Κ", "Λ", "Μ", "Ν", "Ξ", "Ο", "Π", "Ρ", "Σ", "Τ", "Υ", "Φ", "Χ", "Ψ", "Ω"]
greekLetter :: TypeTable -> String
greekLetter table = greekAlphabet !! length table

data Result t = Ok t | Err String

{-  Err constants 
    Not found                       = променливата не е намерена в таблицата с типове 
    Unknown variable: x             = x e все още непозната променлива
    Mismatch of argument types      = неправилни типове при прилагане на функция
    First term is not a function    = първата операнда на прилагането не е функция
-}


search :: String -> [(String, Type)] -> Result Type
search x [] = Err "Not found"
search x ((name, t):rest)
    | name == x = Ok t
    | otherwise = search x rest

typeOf :: TypeTable -> LambdaTerm -> Result (Type, TypeTable)
-- просто (М) променлива - beta reduction
typeOf type_table (Argument x) =
    case search x type_table of
        Ok t  -> Ok (t, type_table)
        Err "Not found" -> Err ("Unknown variable: " ++ x)

-- (MN)
typeOf type_table (Apply m n) =
    case typeOf type_table m of
        Ok (t1, type_table) -> case typeOf type_table n of
            Ok (t2,type_table) -> case t1 of
                Function tArg tRes ->
                    if tArg == t2 then
                        Ok (tRes,type_table)
                    else Err "Mismatch of argument types"
                _ -> Err "First term is not a function"
            Err msg -> Err msg
        Err msg -> Err msg

-- (λx.M)
typeOf type_table (Lambda x m) =
    let new_table = (x, Variable current_greek_letter) : type_table
        in case typeOf new_table m of
            Ok (t, _) -> Ok (Function (Variable x) t, new_table)
            Err msg -> Err msg
    where
        current_greek_letter = show $ greekLetter type_table

main :: IO ()
main = do
    let term = Lambda "x" (Argument "x")
    case typeOf [] term of
        Ok (t, type_table)  -> putStrLn $ "Type of the term: " ++ show t ++ "\n\twhere: \n" ++
            concatMap (\(var, t) -> "\t(" ++ var ++ ", " ++ show t ++ ")\n") type_table
        Err err -> putStrLn $ "Error: " ++ err