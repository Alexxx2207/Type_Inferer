module Inference where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Map                   as TypeTable
import           Environment
import           LambdaTerm
import           TermType
import           Utils

type StateWithSideEffect ttype = StateT Int (Except String) ttype

generateNewVariable :: StateWithSideEffect TermType
generateNewVariable = do
    i <- get
    put $ succ i
    return (TypeVariable ("a" ++ show i))

inferType :: Stack -> LambdaTerm -> StateWithSideEffect (TermType, Substitutions)
inferType stack (Variable x) =
    case TypeTable.lookup x stack of
        Just t  -> return (t, TypeTable.empty)
        Nothing -> throwError $ unboundVariable ++ x

inferType stack (Apply m n) = do
    (tM, sM) <- inferType stack m

    (tN, sN) <- inferType (applySubstritutionEntireStack sM stack) n

    newVarTypeName <- generateNewVariable
    substitutionsApply <- lift $ generateSubstitutions (applySubstitutionSpecificType sN tM) (TypeFunction tN newVarTypeName)
    return (applySubstitutionSpecificType substitutionsApply newVarTypeName, chainSubstitutions substitutionsApply $ chainSubstitutions sN sM)

inferType stack (Lambda [] body) = inferType stack body
inferType stack (Lambda (arg:rest) body) = do
    argType <- generateNewVariable
    (tBody, substitutionsFromBody) <- inferType (TypeTable.insert arg argType stack) (Lambda rest body)
    return (TypeFunction (applySubstitutionSpecificType substitutionsFromBody argType) tBody, substitutionsFromBody)


inferTypeFacade :: LambdaTerm -> Either TermType String
inferTypeFacade e =
    case runExcept $ runStateT (inferType TypeTable.empty e) 0  of
        Right ((ttype, substitutions), _) -> Left (applySubstitutionSpecificType substitutions ttype)
        Left err -> Right err
