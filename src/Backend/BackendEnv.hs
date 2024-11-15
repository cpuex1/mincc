{-# LANGUAGE OverloadedStrings #-}

module Backend.BackendEnv (
    RegID,
    BackendConfig (BackendConfig, argLimit),
    BackendEnv (
        BackendEnv,
        generatedIReg,
        generatedFReg,
        iArgsLen,
        fArgsLen,
        iMap,
        fMap
    ),
    defaultBackendEnv,
    BackendState,
    BackendStateT,
    liftB,
    runBackendStateT,
    genTempIReg,
    genTempFReg,
    genIReg,
    genFReg,
    findI,
    findF,
) where

import Backend.Asm (Register (TempReg))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT, evalStateT, gets, modify)
import Data.Functor.Identity (Identity)
import Error (CompilerError (OtherError))
import Syntax (Ident)

type RegID = Int

newtype BackendConfig = BackendConfig
    { argLimit :: Int
    }
    deriving (Show, Eq)

data BackendEnv = BackendEnv
    { generatedIReg :: Int
    , generatedFReg :: Int
    , iArgsLen :: Int
    , fArgsLen :: Int
    , iMap :: [(Ident, Register RegID Int)]
    , fMap :: [(Ident, Register RegID Float)]
    }
    deriving (Show, Eq)

defaultBackendEnv :: BackendEnv
defaultBackendEnv =
    BackendEnv
        { generatedIReg = 0
        , generatedFReg = 0
        , iArgsLen = 0
        , fArgsLen = 0
        , iMap = []
        , fMap = []
        }

type BackendStateT m = ReaderT BackendConfig (ExceptT CompilerError (StateT BackendEnv m))
type BackendState = BackendStateT Identity

runBackendStateT :: (Monad m) => BackendStateT m a -> BackendConfig -> m (Either CompilerError a)
runBackendStateT s config =
    evalStateT (runExceptT $ runReaderT s config) defaultBackendEnv

liftB :: (Monad m) => m a -> BackendStateT m a
liftB = lift . lift . lift

genTempIReg :: (Monad m) => BackendStateT m (Register RegID Int)
genTempIReg = do
    env <- get
    modify $ \e -> e{generatedIReg = generatedIReg e + 1}
    return $ TempReg $ generatedIReg env

genTempFReg :: (Monad m) => BackendStateT m (Register RegID Float)
genTempFReg = do
    env <- get
    modify $ \e -> e{generatedFReg = generatedFReg e + 1}
    return $ TempReg $ generatedFReg env

genIReg :: (Monad m) => Ident -> BackendStateT m (Register RegID Int)
genIReg ident = do
    reg <- genTempIReg
    modify $ \e ->
        e
            { iMap = (ident, reg) : iMap e
            }
    pure reg

genFReg :: (Monad m) => Ident -> BackendStateT m (Register RegID Float)
genFReg ident = do
    reg <- genTempFReg
    modify $ \e ->
        e
            { fMap = (ident, reg) : fMap e
            }
    pure reg

findI :: (Monad m) => Ident -> BackendStateT m (Register RegID Int)
findI ident = do
    regID <- gets (lookup ident . iMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError "Detected an unknown non-float identifier."

findF :: (Monad m) => Ident -> BackendStateT m (Register RegID Float)
findF ident = do
    regID <- gets (lookup ident . fMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError "Detected an unknown float identifier."
