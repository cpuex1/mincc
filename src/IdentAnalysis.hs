{-# LANGUAGE OverloadedStrings #-}

module IdentAnalysis (
    IdentProp (IdentProp, typeOf, constant, isClosure),
    IdentE,
    defaultIdentE,
    IdentEnvT (IdentEnvT, runIdentEnvT),
    IdentEnv,
    registerProp,
    searchProp,
    getTyOf,
    identState,
    updateProp,
    genNewVar,
    loadTypeEnv,
    reportEnv,
) where

import Control.Monad.Identity (Identity)
import Control.Monad.Trans
import Data.Text (Text, pack)
import Display (display)
import Syntax
import TypeInferrer (TypeEnv (variables), removeVar)
import Typing (Ty, TypeKind (TUnit, TVar))

data IdentProp
    = IdentProp
    { typeOf :: Ty
    , constant :: Maybe Literal
    , isClosure :: Bool
    }
    deriving (Show, Eq)

data IdentE = IdentE {identProps :: [(Ident, IdentProp)], nextIdent :: Int}
    deriving (Show, Eq)

defaultIdentE :: IdentE
defaultIdentE = IdentE [] 0

newtype IdentEnvT m a = IdentEnvT {runIdentEnvT :: IdentE -> m (a, IdentE)}
type IdentEnv a = IdentEnvT Identity a

instance (Functor m) => Functor (IdentEnvT m) where
    fmap f m = IdentEnvT $ \s -> do
        fmap (\ ~(a, s') -> (f a, s')) (runIdentEnvT m s)

instance (Functor m, Monad m) => Applicative (IdentEnvT m) where
    pure a = IdentEnvT $ \s -> return (a, s)
    IdentEnvT left <*> IdentEnvT right = IdentEnvT $ \s -> do
        ~(f, s') <- left s
        ~(a, s'') <- right s'
        return (f a, s'')

instance (Monad m) => Monad (IdentEnvT m) where
    m >>= k = IdentEnvT $ \s -> do
        ~(a, s') <- runIdentEnvT m s
        runIdentEnvT (k a) s'

instance MonadTrans IdentEnvT where
    lift m = IdentEnvT $ \s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (IdentEnvT m) where
    liftIO = lift . liftIO

getEnv :: (Monad m) => IdentEnvT m IdentE
getEnv = IdentEnvT $ \s -> return (s, s)

modifyEnv :: (Monad m) => (IdentE -> IdentE) -> IdentEnvT m ()
modifyEnv f = IdentEnvT $ \s -> return ((), f s)

registerProp :: (Monad m) => Ident -> IdentProp -> IdentEnvT m ()
registerProp ident prop = do
    found <- searchProp ident
    case found of
        Just _ -> pure ()
        Nothing -> modifyEnv $ \env ->
            env
                { identProps = (ident, prop) : identProps env
                }

searchProp :: (Monad m) => Ident -> IdentEnvT m (Maybe IdentProp)
searchProp ident =
    IdentEnvT $ \env -> return (lookup ident (identProps env), env)

{- | Get the type of an identifier.
If the identifier is not found, return `TUnit`.
-}
getTyOf :: (Monad m) => Ident -> IdentEnvT m Ty
getTyOf ident = do
    found <- searchProp ident
    case found of
        Just prop -> pure (typeOf prop)
        Nothing -> pure TUnit

identState :: (Monad m) => Ident -> IdentEnvT m TypedState
identState ident = do
    ty <- getTyOf ident
    pure $ TypedState ty $ identLoc ident

updateProp :: (Monad m) => Ident -> (IdentProp -> IdentProp) -> IdentEnvT m ()
updateProp ident f =
    modifyEnv $ \env ->
        env
            { identProps = map (\(i, p) -> if i == ident then (i, f p) else (i, p)) (identProps env)
            }

genNewVar :: (Monad m) => Ty -> IdentEnvT m Ident
genNewVar ty = do
    newIdent <- IdentEnvT $ \env ->
        return
            ( CompilerGenerated (nextIdent env)
            , env
                { nextIdent = nextIdent env + 1
                }
            )
    _ <- registerProp newIdent (IdentProp ty Nothing False)
    pure newIdent

loadTypeEnv :: (Monad m) => TypeEnv -> IdentEnvT m ()
loadTypeEnv typeEnv = do
    mapM_
        ( \(ident, tId) ->
            registerProp ident (IdentProp (removeVar typeEnv (TVar tId)) Nothing False)
        )
        vars
  where
    vars = variables typeEnv

reportEnv :: (Monad m) => IdentEnvT m [Text]
reportEnv = do
    map
        ( \(ident, IdentProp ty c isC) ->
            display ident
                <> " : "
                <> display ty
                <> " (Const: "
                <> pack (show c)
                <> ", IsClosure: "
                <> if isC
                    then "Yes"
                    else
                        "No"
                            <> ")"
        )
        . identProps
        <$> getEnv
