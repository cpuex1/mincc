{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module BackEnd.BackendEnv (
    RegConfig (..),
    RegContext (..),
    RegVariant,
    selectTy,
    chooseTy,
    updateVariant,
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
    findRegOrImm,
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
import IR (RegID, RegOrImm (Imm, Reg), RegType (RFloat, RInt), Register (SavedReg))
import MiddleEnd.Globals (GlobalProp (globalOffset), GlobalTable (globalTable))
import Syntax (Ident (ExternalIdent))
import Prelude hiding (lookup)

newtype RegConfig ty
    = RegConfig {regLimit :: Int}
    deriving (Show, Eq)

data RegContext ty
    = RegContext
    { generatedReg :: Int
    , argsLength :: Int
    , registerMap :: Map Ident (Register RegID ty)
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

data RegVariant f
    = RegVariant
    { iVariant :: f Int
    , fVariant :: f Float
    }

deriving instance (Show (f Int), Show (f Float)) => Show (RegVariant f)
deriving instance (Eq (f Int), Eq (f Float)) => Eq (RegVariant f)

selectTy :: RegVariant f -> RegType a -> f a
selectTy (RegVariant i _) RInt = i
selectTy (RegVariant _ f) RFloat = f

chooseTy :: RegType a -> (f a -> b) -> RegVariant f -> b
chooseTy rTy func variant = func $ selectTy variant rTy

updateVariant :: RegType a -> (f a -> f a) -> RegVariant f -> RegVariant f
updateVariant RInt func variant = variant{iVariant = func $ iVariant variant}
updateVariant RFloat func variant = variant{fVariant = func $ fVariant variant}

newtype BackendConfig = BackendConfig
    { regConfig :: RegVariant RegConfig
    }
    deriving (Show, Eq)

createBackendConfig :: Int -> Int -> BackendConfig
createBackendConfig iLimit fLimit =
    BackendConfig $
        RegVariant
            { iVariant = RegConfig iLimit
            , fVariant = RegConfig fLimit
            }

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
            RegVariant
                { iVariant = defaultRegContext
                , fVariant = defaultRegContext
                }
        }

type BackendStateT m = ReaderT BackendConfig (ExceptT CompilerError (StateT BackendEnv m))
type BackendState = BackendStateT Identity

runBackendStateT :: (Monad m) => BackendStateT m a -> BackendConfig -> GlobalTable -> m (Either CompilerError a)
runBackendStateT s config table =
    evalStateT (runExceptT $ runReaderT s config) $ defaultBackendEnv table

liftB :: (Monad m) => m a -> BackendStateT m a
liftB = lift . lift . lift

genTempReg :: (Monad m) => RegType a -> BackendStateT m (Register RegID a)
genTempReg rTy = do
    ctx <- gets regContext
    modify $ \ctx' ->
        ctx'
            { regContext =
                updateVariant
                    rTy
                    ( \ctx'' ->
                        ctx''
                            { generatedReg = generatedReg ctx'' + 1
                            }
                    )
                    $ regContext ctx'
            }
    pure $ SavedReg $ chooseTy rTy generatedReg ctx

genReg :: (Monad m) => RegType a -> Ident -> BackendStateT m (Register RegID a)
genReg rTy ident = do
    reg <- genTempReg rTy
    modify $ \ctx ->
        ctx
            { regContext =
                updateVariant
                    rTy
                    ( \ctx' ->
                        ctx'
                            { registerMap = insert ident reg $ registerMap ctx'
                            }
                    )
                    $ regContext ctx
            }
    pure reg

findReg :: (Monad m) => RegType a -> Ident -> BackendStateT m (Register RegID a)
findReg rTy ident = do
    regID <- gets (chooseTy rTy (lookup ident . registerMap) . regContext)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown identifier named " <> display ident <> "."

findRegOrImm :: (Monad m) => RegType a -> Ident -> BackendStateT m (RegOrImm RegID a)
findRegOrImm RInt (ExternalIdent ext) =
    Imm . globalOffset <$> findGlobal ext
findRegOrImm rTy ident = Reg <$> findReg rTy ident

-- | Finds a global variable by its name.
findGlobal :: (Monad m) => Text -> BackendStateT m GlobalProp
findGlobal name = do
    table <- gets globals
    case M.lookup name $ globalTable table of
        Just prop -> pure prop
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown global variable named " <> name <> "."
