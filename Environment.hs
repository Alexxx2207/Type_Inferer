module Environment where

import           Control.Monad.Except
import           Data.Map             as TypeTable
import           Data.Maybe           (fromMaybe)
import           TermType             (TermType (..))
import           Utils


type Stack = TypeTable.Map String TermType
type Substitutions = TypeTable.Map String TermType

-- <полагания, стар тип> -> нов тип
applySubstitutionSpecificType :: Substitutions -> TermType -> TermType
applySubstitutionSpecificType substitutions old@(TypeVariable x)      = fromMaybe old (TypeTable.lookup x substitutions)
applySubstitutionSpecificType substitutions (TypeFunction arg1 res1)  = TypeFunction (applySubstitutionSpecificType substitutions arg1) (applySubstitutionSpecificType substitutions res1)

-- <полагания, стар стек> -> нов стек
applySubstritutionEntireStack :: Substitutions -> Stack -> Stack
applySubstritutionEntireStack substitutions = TypeTable.map (applySubstitutionSpecificType substitutions)

chainSubstitutions :: Substitutions -> Substitutions -> Substitutions
chainSubstitutions oldSubs newSubs = TypeTable.union (TypeTable.map (applySubstitutionSpecificType oldSubs) newSubs) oldSubs


generateSubstitutions :: TermType -> TermType -> Except String Substitutions
generateSubstitutions old@(TypeVariable typeName) typeToUnifyTo
    | typeToUnifyTo == old = return TypeTable.empty
    | contains typeName typeToUnifyTo = throwError $ cyclicDefinition typeName typeToUnifyTo
    | otherwise = return (TypeTable.singleton typeName typeToUnifyTo)

generateSubstitutions typeToUnifyTo old@(TypeVariable typeName)
    | typeToUnifyTo == old = return TypeTable.empty
    | contains typeName typeToUnifyTo = throwError $ cyclicDefinition typeName typeToUnifyTo
    | otherwise = return (TypeTable.singleton typeName typeToUnifyTo)

generateSubstitutions (TypeFunction arg1 res1) (TypeFunction arg2 res2) = do
    s1 <- generateSubstitutions arg1 arg2
    s2 <- generateSubstitutions (applySubstitutionSpecificType s1 res1) (applySubstitutionSpecificType s1 res2)
    return $ chainSubstitutions s2 s1

contains :: String -> TermType -> Bool
contains typeName (TypeVariable x)         = x == typeName
contains typeName (TypeFunction arg1 res1) = contains typeName arg1 || contains typeName res1
