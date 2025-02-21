{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.BackendEnv (
    RegConfig (..),
    RegContext (..),
    BackendConfig (..),
    BackendEnv (..),
    createBackendConfig,
    defaultBackendEnv,
    BackendState,
    BackendStateT,
    liftB,
    runBackendStateT,
    genReg,
    genTempReg,
    findReg,
    registerReg,
    findRegOrImm,
    updateRegContext,
    findGlobal,
) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Functor.Identity (Identity)
import Data.Map (Map, insert, lookup)
import qualified Data.Map as M
import Data.Text (Text)
import Display (display)
import Error (CompilerError (OtherError))
import MiddleEnd.Globals (GlobalProp (globalOffset), GlobalTable (globalTable))
import Registers (
    RegOrImm (Imm, Reg),
    RegTuple (createRT),
    RegType (RInt),
    RegVariant,
    Register (Register),
    RegisterKind (SavedReg),
    buildRT,
    updateRT,
    (#!!),
 )
import Syntax (Ident (ExternalIdent))
import Prelude hiding (lookup)

data RegConfig ty = RegConfig
    deriving (Show, Eq)

data RegContext ty
    = RegContext
    { generatedReg :: Int
    , argsLength :: Int
    , registerMap :: Map Ident (Register ty)
    , usedRegLen :: Int
    }
    deriving (Show, Eq)

defaultRegContext :: RegContext ty
defaultRegContext =
    RegContext
        { generatedReg = 0
        , argsLength = 0
        , registerMap = M.empty
        , usedRegLen = 0
        }

newtype BackendConfig = BackendConfig
    { regConfig :: RegVariant RegConfig
    }
    deriving (Show, Eq)

createBackendConfig :: BackendConfig
createBackendConfig =
    BackendConfig $ createRT RegConfig RegConfig

data BackendEnv = BackendEnv
    { globals :: GlobalTable
    , regContext :: RegVariant RegContext
    }
    deriving (Show, Eq)

defaultBackendEnv :: GlobalTable -> BackendEnv
defaultBackendEnv table =
    BackendEnv
        { globals = table
        , regContext =
            buildRT (const defaultRegContext)
        }

type BackendStateT m = ReaderT BackendConfig (ExceptT CompilerError (StateT BackendEnv m))
type BackendState = BackendStateT Identity

runBackendStateT :: (Monad m) => BackendStateT m a -> BackendConfig -> GlobalTable -> m (Either CompilerError a)
runBackendStateT s config table =
    evalStateT (runExceptT $ runReaderT s config) $ defaultBackendEnv table

liftB :: (Monad m) => m a -> BackendStateT m a
liftB = lift . lift . lift

genTempReg :: (Monad m) => RegType a -> BackendStateT m (Register a)
genTempReg rTy = do
    ctx <- gets regContext
    modify $ \ctx' ->
        ctx'
            { regContext =
                updateRT
                    rTy
                    ( \ctx'' ->
                        ctx''
                            { generatedReg = generatedReg ctx'' + 1
                            }
                    )
                    $ regContext ctx'
            }
    pure $ Register rTy $ SavedReg $ generatedReg $ ctx #!! rTy

genReg :: (Monad m) => RegType a -> Ident -> BackendStateT m (Register a)
genReg rTy ident = do
    reg <- genTempReg rTy
    modify $ \ctx ->
        ctx
            { regContext =
                updateRT
                    rTy
                    ( \ctx' ->
                        ctx'
                            { registerMap = insert ident reg $ registerMap ctx'
                            }
                    )
                    $ regContext ctx
            }
    pure reg

findReg :: (Monad m) => RegType a -> Ident -> BackendStateT m (Register a)
findReg rTy ident = do
    regID <- gets ((lookup ident . registerMap) . (#!! rTy) . regContext)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown identifier named " <> display ident <> "."

findRegOrImm :: (Monad m) => RegType a -> Ident -> BackendStateT m (RegOrImm a)
findRegOrImm RInt (ExternalIdent ext) =
    Imm RInt . globalOffset <$> findGlobal ext
findRegOrImm rTy ident = Reg <$> findReg rTy ident

registerReg :: (Monad m) => Ident -> Register a -> BackendStateT m ()
registerReg ident (Register rTy kind) = do
    modify $ \ctx ->
        ctx
            { regContext =
                updateRT
                    rTy
                    ( \ctx' ->
                        ctx'
                            { registerMap = insert ident (Register rTy kind) $ registerMap ctx'
                            }
                    )
                    $ regContext ctx
            }

-- | Updates the register context.
updateRegContext :: (Monad m) => RegType a -> (RegContext a -> RegContext a) -> BackendStateT m ()
updateRegContext rTy f = do
    modify $ \ctx ->
        ctx
            { regContext =
                updateRT rTy f $ regContext ctx
            }

-- | Finds a global variable by its name.
findGlobal :: (Monad m) => Text -> BackendStateT m GlobalProp
findGlobal name = do
    table <- gets globals
    case M.lookup name $ globalTable table of
        Just prop -> pure prop
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown global variable named " <> name <> "."
