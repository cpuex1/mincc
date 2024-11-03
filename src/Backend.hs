{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend (
    loadFunctions,
) where

import Asm
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get), State, gets, modify, runState)
import Data.Text (pack)
import Display (display)
import Error (CompilerError (OtherError))
import Syntax
import Typing (TypeKind (TFloat))

type RegID = Int

newtype BackendConfig = BackendConfig
    { argLimit :: Int
    }
    deriving (Show, Eq)

data BackendEnv = BackendEnv
    { generatedIReg :: Int
    , generatedFReg :: Int
    , generatedLabel :: Int
    , codeBlocks :: [CodeBlock Loc RegID]
    , iMap :: [(Ident, Register RegID Int)]
    , fMap :: [(Ident, Register RegID Float)]
    , currentLabel :: InstLabel
    , currentTerm :: InstTerm Loc RegID
    , currentRet :: Register RegID Int
    , instBuffer :: [Inst Loc RegID]
    }
    deriving (Show, Eq)

defaultBackendEnv :: BackendEnv
defaultBackendEnv =
    BackendEnv
        { generatedIReg = 0
        , generatedFReg = 0
        , generatedLabel = 0
        , codeBlocks = []
        , iMap = []
        , fMap = []
        , currentLabel = ""
        , currentTerm = Nop
        , currentRet = ZeroReg
        , instBuffer = []
        }

type BackendState = ReaderT BackendConfig (ExceptT CompilerError (State BackendEnv))

genIReg :: BackendState (Register RegID Int)
genIReg = do
    env <- get
    modify $ \e -> e{generatedIReg = generatedIReg e + 1}
    return $ TempReg $ generatedIReg env

genFReg :: BackendState (Register RegID Float)
genFReg = do
    env <- get
    modify $ \e -> e{generatedFReg = generatedFReg e + 1}
    return $ TempReg $ generatedFReg env

genLabel :: BackendState InstLabel
genLabel = do
    env <- get
    modify $ \e -> e{generatedLabel = generatedLabel e + 1}
    return $ "__label_" <> pack (show $ generatedLabel env)

findI :: Ident -> BackendState (Register RegID Int)
findI ident = do
    regID <- gets (lookup ident . iMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown identifier named " <> display ident <> "."

findF :: Ident -> BackendState (Register RegID Float)
findF ident = do
    regID <- gets (lookup ident . fMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown identifier named " <> display ident <> "."

flushInstBuffer :: BackendState ()
flushInstBuffer =
    modify $ \env ->
        env
            { codeBlocks = codeBlocks env ++ [CodeBlock (currentLabel env) (instBuffer env) (currentTerm env)]
            , instBuffer = []
            }

addInst :: Inst Loc RegID -> BackendState ()
addInst inst = modify $ \env -> env{instBuffer = instBuffer env ++ [inst]}

fromState :: TypedState -> Loc
fromState (TypedState _ pos) = fromSourcePos pos

toInstU :: ClosureExpr -> BackendState ()
toInstU (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstU (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstU (Const _ LUnit) = pure ()
toInstU (Put{}) =
    throwError $ OtherError "Array is not supported."
toInstU (ClosureApp{}) =
    throwError $ OtherError "Closure is not supported."
toInstU (DirectApp state func args) = do
    args' <- mapM findI args
    argLimit' <- asks argLimit
    when (length args' > argLimit') $ do
        throwError $ OtherError "The number of arguments exceeds the limit."
    mapM_ (\(i, arg) -> addInst $ InstMov (fromState state) (Reg (ArgsReg i)) (Reg arg)) $ zip [0 ..] args'
    addInst $ InstCall (fromState state) $ display func
toInstU _ =
    throwError $ OtherError "The expression cannot have type unit."

toInstI :: Register RegID Int -> ClosureExpr -> BackendState ()
toInstI _ (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstI _ (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstI reg (Const state (LInt i)) =
    addInst $ InstMov (fromState state) (Reg reg) (Imm i)
toInstI reg (Const state (LBool b)) =
    addInst $ InstMov (fromState state) (Reg reg) (Imm $ if b then 1 else 0)
toInstI reg (Unary state Not operand) = do
    operand' <- findI operand
    addInst $ InstRelationOp (fromState state) Eq (Reg reg) (Reg ZeroReg) (Reg operand')
toInstI reg (Unary state Neg operand) = do
    operand' <- findI operand
    addInst $ InstIntBinOp (fromState state) Sub (Reg reg) (Reg ZeroReg) (Reg operand')
toInstI reg (Binary state (RelationOp op) operand1 operand2) = do
    operand1' <- findI operand1
    operand2' <- findI operand2
    addInst $ InstRelationOp (fromState state) op (Reg reg) (Reg operand1') (Reg operand2')
toInstI reg (Binary state (IntOp op) operand1 operand2) = do
    operand1' <- findI operand1
    operand2' <- findI operand2
    addInst $ InstIntBinOp (fromState state) op (Reg reg) (Reg operand1') (Reg operand2')
toInstI reg (Var state ident) = do
    regID <- findI ident
    when (Reg reg /= Reg regID) $
        addInst $
            InstMov (fromState state) (Reg reg) (Reg regID)
toInstI _ (Tuple{}) =
    throwError $ OtherError "Tuple is not supported yet."
toInstI _ (ArrayCreate{}) =
    throwError $ OtherError "Array is not supported yet."
toInstI _ (Get{}) =
    throwError $ OtherError "Array is not supported yet."
toInstI _ (MakeClosure{}) =
    throwError $ OtherError "Closure is not supported yet."
toInstI reg (DirectApp state func args) = do
    toInstU (DirectApp state func args)
    when (Reg reg /= Reg RetReg) $
        addInst $
            InstMov (fromState state) (Reg reg) $
                Reg RetReg
toInstI _ (ClosureApp{}) =
    throwError $ OtherError "Closure is not supported yet."
toInstI _ _ =
    throwError $ OtherError "The expression cannot be represented as int."

toInstF :: Register RegID Float -> ClosureExpr -> BackendState ()
toInstF _ (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstF _ (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstF reg (Const state (LFloat f)) =
    addInst $ InstFMov (fromState state) (Reg reg) (Imm f)
toInstF reg (Unary state FNeg operand) = do
    operand' <- findF operand
    addInst $ InstFloatBinOp (fromState state) FSub (Reg reg) (Reg ZeroReg) (Reg operand')
toInstF reg (Binary state (FloatOp op) operand1 operand2) = do
    operand1' <- findF operand1
    operand2' <- findF operand2
    addInst $ InstFloatBinOp (fromState state) op (Reg reg) (Reg operand1') (Reg operand2')
toInstF reg (Var state ident) = do
    regID <- findF ident
    when (Reg reg /= Reg regID) $
        addInst $
            InstFMov (fromState state) (Reg reg) (Reg regID)
toInstF _ (Get{}) =
    throwError $ OtherError "Array is not supported yet."
toInstF reg (DirectApp state func args) = do
    toInstU (DirectApp state func args)
    when (Reg reg /= Reg RetReg) $
        addInst $
            InstFMov (fromState state) (Reg reg) $
                Reg RetReg
toInstF _ _ =
    throwError $ OtherError "The expression cannot have type float."

toAsm :: ClosureExpr -> BackendState ()
toAsm (If state cond thenExpr elseExpr) = do
    cond' <- findI cond

    term <- gets currentTerm

    -- Flush the current code block.
    thenLabel <- genLabel
    elseLabel <- genLabel
    modify $ \env -> env{currentTerm = Branch (fromState state) Eq (Reg cond') (Reg ZeroReg) elseLabel thenLabel}
    flushInstBuffer

    -- Generate the "then" code block.
    modify $ \env -> env{currentLabel = thenLabel, currentTerm = term}
    toAsm thenExpr

    -- Generate the "else" code block.
    modify $ \env -> env{currentLabel = elseLabel, currentTerm = term}
    toAsm elseExpr
toAsm (Let _ PUnit (If state cond thenExpr elseExpr) body) = do
    cond' <- findI cond

    term <- gets currentTerm

    -- Flush the current code block.
    thenLabel <- genLabel
    elseLabel <- genLabel
    endLabel <- genLabel
    modify $ \env -> env{currentTerm = Branch (fromState state) Eq (Reg cond') (Reg ZeroReg) elseLabel thenLabel}
    flushInstBuffer

    -- Generate the "then" code block.
    modify $ \env ->
        env
            { currentLabel = thenLabel
            , currentTerm = Jmp (fromState state) endLabel
            }
    toAsm thenExpr

    -- Generate the "else" code block.
    modify $ \env ->
        env
            { currentLabel = elseLabel
            , currentTerm = Jmp (fromState state) endLabel
            }
    toAsm elseExpr

    -- Generate the "end" code block.
    modify $ \env ->
        env
            { currentLabel = endLabel
            , currentTerm = term
            }
    toAsm body
toAsm (Let _ (PVar v) (If state cond thenExpr elseExpr) body) = do
    cond' <- findI cond

    term <- gets currentTerm
    retReg <- gets currentRet

    phiReg <- genIReg
    modify $ \env -> env{iMap = (v, phiReg) : iMap env}

    -- Flush the current code block.
    thenLabel <- genLabel
    elseLabel <- genLabel
    endLabel <- genLabel
    modify $ \env -> env{currentTerm = Branch (fromState state) Eq (Reg cond') (Reg ZeroReg) elseLabel thenLabel}
    flushInstBuffer

    -- Generate the "then" code block.
    modify $ \env ->
        env
            { currentLabel = thenLabel
            , currentTerm = Jmp (fromState state) endLabel
            , currentRet = phiReg
            }
    toAsm thenExpr

    -- Generate the "else" code block.
    modify $ \env ->
        env
            { currentLabel = elseLabel
            , currentTerm = Jmp (fromState state) endLabel
            , currentRet = phiReg
            }
    toAsm elseExpr

    -- Generate the "end" code block.
    modify $ \env ->
        env
            { currentLabel = endLabel
            , currentTerm = term
            , currentRet = retReg
            }
    toAsm body
toAsm (Let _ PUnit expr body) = do
    toInstU expr
    toAsm body
toAsm (Let _ (PVar v) expr body) = do
    if getType (getExprState expr) == TFloat
        then do
            reg <- genFReg
            toInstF reg expr
            modify $ \env -> env{fMap = (v, reg) : fMap env}
            toAsm body
        else do
            reg <- genIReg
            toInstI reg expr
            modify $ \env -> env{iMap = (v, reg) : iMap env}
            toAsm body
toAsm expr = do
    retReg <- gets currentRet
    toInstI retReg expr
    flushInstBuffer

loadFunctions :: BackendConfig -> [Function] -> Either CompilerError [CodeBlock Loc RegID]
loadFunctions config functions =
    case result of
        (Left err, _) -> Left err
        (Right _, env) -> Right $ codeBlocks env
  where
    result =
        runState
            ( runExceptT $ runReaderT (mapM_ loadFunctionAsAsm functions) config
            )
            defaultBackendEnv

loadFunctionAsAsm :: Function -> BackendState ()
loadFunctionAsAsm (Function state _ name freeVars' boundedArgs' body) = do
    modify $ \env ->
        env
            { generatedIReg = 0
            , generatedFReg = 0
            , iMap = argReg
            , fMap = []
            , currentLabel = display name
            , currentTerm = Return (fromState state)
            , currentRet = RetReg
            , instBuffer = []
            }
    toAsm body
  where
    freeReg = zipWith (\v i -> (v, ArgsReg i)) freeVars' [0 ..]
    boundedReg =
        zipWith
            (\v i -> (v, ArgsReg i))
            boundedArgs'
            [(length freeReg) ..]
    argReg = freeReg ++ boundedReg
