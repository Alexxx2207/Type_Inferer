module Environment where

import           Data.Map   as TypeTable
import           Data.Maybe (fromMaybe)
import           TermType   (TermType (..))
import           Utils


type Stack = TypeTable.Map String TermType
type Substitutions = TypeTable.Map String TermType

-- <полагания, стар тип> -> нов тип
-- прилага полаганията, но само за един конкретен тип
-- полезно е, когато свършва оценяването на връх от дървото
appSubstToType :: Substitutions -> TermType -> TermType
appSubstToType substitutions old@(TypeVariable x)      = fromMaybe old (TypeTable.lookup x substitutions)
appSubstToType substitutions (TypeFunction arg1 res1)  = TypeFunction (appSubstToType substitutions arg1) (appSubstToType substitutions res1)

-- <полагания, стар стек> -> нов стек
-- прилага полаганията върху целия стек
-- полезно е, когато при апликация искаме да променит тип - резултат от предичшно оценяване преди да оценим нов израз
appSubstToStack :: Substitutions -> Stack -> Stack
appSubstToStack substitutions = TypeTable.map (appSubstToType substitutions)

-- <полагания, стар стек> -> нов стек
-- прилага полаганията върху полагания
-- целта е да избегнем частни случаи, при които се разчита на потредбата на полаганията в таблицата при обхождане
-- пример: полагане на полагането не може да бъде извършено преди първото полагане. Искаме гаранция, че ще се случат в правилен ред
chainSubst :: Substitutions -> Substitutions -> Substitutions
chainSubst oldSubs newSubs = TypeTable.union (TypeTable.map (appSubstToType oldSubs) newSubs) oldSubs


-- генерира полагания така че да си паснат типовете
genSubst :: TermType -> TermType -> Result Substitutions
genSubst old@(TypeVariable tName) target
    | target == old = Ok TypeTable.empty
    | contains tName target = Err $ cyclicDefinition tName target
    | otherwise = Ok  (TypeTable.singleton tName target)

genSubst target old@(TypeVariable tName)
    | target == old = Ok TypeTable.empty
    | contains tName target = Err $ cyclicDefinition tName target
    | otherwise = Ok (TypeTable.singleton tName target)

genSubst (TypeFunction arg1 res1) (TypeFunction arg2 res2) =
    case genSubst arg1 arg2 of
        Ok argSubst ->
            case genSubst (appSubstToType argSubst res1) (appSubstToType argSubst res2) of
                Ok retSubst -> Ok $ chainSubst retSubst argSubst
                Err msg     -> Err msg
        Err msg -> Err msg


-- проверява дали не се среща даден тип в типа на израз
-- Идеята ми е че може да се случи така, че да дефинираме един тип, чрез друг тип, който друг тип се дефинира с единия тип
contains :: String -> TermType -> Bool
contains tName (TypeVariable x)         = x == tName
contains tName (TypeFunction arg1 res1) = contains tName arg1 || contains tName res1
