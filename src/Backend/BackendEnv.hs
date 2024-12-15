{-# LANGUAGE OverloadedStrings #-}

module Backend.BackendEnv (
    RegID,
    BackendConfig (..),
    BackendEnv (..),
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
    findI',
    findF,
    findGlobal,
) where

import Backend.Asm (RegID, RegOrImm (Imm, Reg), Register (SavedReg))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT, evalStateT, gets, modify)
import Data.Functor.Identity (Identity)
import Data.Map as M
import Data.Text (Text)
import Display (display)
import Error (CompilerError (OtherError))
import Globals (GlobalProp (globalOffset), GlobalTable (globalTable))
import Syntax (Ident (ExternalIdent))

data BackendConfig = BackendConfig
    { iRegLimit :: Int
    , fRegLimit :: Int
    }
    deriving (Show, Eq)

data BackendEnv = BackendEnv
    { globals :: GlobalTable
    , generatedIReg :: Int
    , generatedFReg :: Int
    , iArgsLen :: Int
    , fArgsLen :: Int
    , iMap :: [(Ident, Register RegID Int)]
    , fMap :: [(Ident, Register RegID Float)]
    , usedIRegLen :: Int
    , usedFRegLen :: Int
    }
    deriving (Show, Eq)

defaultBackendEnv :: GlobalTable -> BackendEnv
defaultBackendEnv table =
    BackendEnv
        { globals = table
        , generatedIReg = 0
        , generatedFReg = 0
        , iArgsLen = 0
        , fArgsLen = 0
        , iMap = []
        , fMap = []
        , usedIRegLen = 0
        , usedFRegLen = 0
        }

type BackendStateT m = ReaderT BackendConfig (ExceptT CompilerError (StateT BackendEnv m))
type BackendState = BackendStateT Identity

runBackendStateT :: (Monad m) => BackendStateT m a -> BackendConfig -> GlobalTable -> m (Either CompilerError a)
runBackendStateT s config table =
    evalStateT (runExceptT $ runReaderT s config) $ defaultBackendEnv table

liftB :: (Monad m) => m a -> BackendStateT m a
liftB = lift . lift . lift

genTempIReg :: (Monad m) => BackendStateT m (Register RegID Int)
genTempIReg = do
    env <- get
    modify $ \e -> e{generatedIReg = generatedIReg e + 1}
    return $ SavedReg $ generatedIReg env

genTempFReg :: (Monad m) => BackendStateT m (Register RegID Float)
genTempFReg = do
    env <- get
    modify $ \e -> e{generatedFReg = generatedFReg e + 1}
    return $ SavedReg $ generatedFReg env

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
    regID <- gets (Prelude.lookup ident . iMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown non-float identifier named " <> display ident <> "."

findI' :: (Monad m) => Ident -> BackendStateT m (RegOrImm RegID Int)
findI' (ExternalIdent ident) = do
    Imm . globalOffset <$> findGlobal ident
findI' ident = Reg <$> findI ident

findF :: (Monad m) => Ident -> BackendStateT m (Register RegID Float)
findF ident = do
    regID <- gets (Prelude.lookup ident . fMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown float identifier named " <> display ident <> "."

-- | Finds a global variable by its name.
findGlobal :: (Monad m) => Text -> BackendStateT m GlobalProp
findGlobal name = do
    table <- gets globals
    case M.lookup name $ globalTable table of
        Just prop -> pure prop
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown global variable named " <> name <> "."
