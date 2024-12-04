{-# LANGUAGE GADTs #-}

module CNorm (cNormalize) where
import Syntax (KExpr, CExpr, Expr(..), Pattern (PVar), Operand (OIdent, OLiteral))
import IdentAnalysis (IdentEnvT, asOperand, updateProp, IdentProp (constant))

cNormalize :: (Monad m) => KExpr -> IdentEnvT m CExpr
cNormalize (Const s lit) = pure $ Const s lit
cNormalize (Unary s op ident) = do
    operand <- asOperand ident
    pure $ Unary s op operand
cNormalize (Binary s op lhs rhs) = do
    lhs' <- asOperand lhs
    rhs' <- asOperand rhs
    pure $ Binary s op lhs' rhs'
cNormalize (If s cond thenE elseE) = do
    cond' <- asOperand cond
    thenE' <- cNormalize thenE
    elseE' <- cNormalize elseE
    pure $ If s cond' thenE' elseE'
cNormalize (Let _ (PVar v) (Const _ lit) body) = do
    updateProp v (\prop -> prop { constant = Just lit })
    cNormalize body
cNormalize (Let s pat value body) = do
    value' <- cNormalize value
    case (pat, value') of
        (PVar v, Const _ lit) -> do
            updateProp v (\prop -> prop { constant = Just lit })
            cNormalize body
        _ -> do
            body' <- cNormalize body
            pure $ Let s pat value' body'
cNormalize (Var s v) = do
    operand <- asOperand v
    case operand of
        OLiteral lit -> pure $ Const s lit
        OIdent ident -> pure $ Var s ident
cNormalize (App s func args) = do
    func' <- asOperand func
    args' <- mapM asOperand args
    pure $ App s func' args'
cNormalize (Tuple s values) = do
    values' <- mapM asOperand values
    pure $ Tuple s values'
cNormalize (ArrayCreate s size value) = do
    size' <- asOperand size
    value' <- asOperand value
    pure $ ArrayCreate s size' value'
cNormalize (Get s array idx) = do
    array' <- asOperand array
    idx' <- asOperand idx
    pure $ Get s array' idx'
cNormalize (Put s array idx value) = do
    array' <- asOperand array
    idx' <- asOperand idx
    value' <- asOperand value
    pure $ Put s array' idx' value'
