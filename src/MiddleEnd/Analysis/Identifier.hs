{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.Analysis.Identifier (
    IdentProp (..),
    IdentContext (..),
    IdentEnvT,
    defaultIdentContext,
    registerProp,
    searchProp,
    removeProp,
    getTyOf,
    asConstant,
    identState,
    updateProp,
    genNewVar,
    loadTypeEnv,
    reportEnv,
) where

import Control.Monad.State (StateT, gets, modify)
import Data.Map (Map, adjust, delete, empty, insert, lookup, toList)
import Data.Text (Text, pack)
import Display (display)
import FrontEnd.TypeInferrer (TypeEnv (variables), removeVar)
import Syntax
import Typing (Ty, TypeBase (..))
import Prelude hiding (lookup)

data IdentProp
    = IdentProp
    { typeOf :: Ty
    , constant :: Maybe Literal
    , isClosure :: Bool
    , containsLoop :: Maybe Bool
    }
    deriving (Show, Eq)

data IdentContext = IdentContext
    { identProps :: Map Ident IdentProp
    , nextIdent :: Int
    }
    deriving (Show, Eq)

defaultIdentContext :: IdentContext
defaultIdentContext = IdentContext empty 0

type IdentEnvT m = StateT IdentContext m

registerProp :: (Monad m) => Ident -> IdentProp -> IdentEnvT m ()
registerProp ident prop = do
    found <- searchProp ident
    case found of
        Just _ -> pure ()
        Nothing -> modify $ \env ->
            env
                { identProps = insert ident prop $ identProps env
                }

searchProp :: (Monad m) => Ident -> IdentEnvT m (Maybe IdentProp)
searchProp ident =
    gets identProps >>= \props -> pure (lookup ident props)

removeProp :: (Monad m) => Ident -> IdentEnvT m ()
removeProp ident =
    modify $ \env -> env{identProps = delete ident $ identProps env}

{- | Get the type of an identifier.
If the identifier is not found, return `TUnit`.
-}
getTyOf :: (Monad m) => Ident -> IdentEnvT m Ty
getTyOf ident = do
    found <- searchProp ident
    case found of
        Just prop -> pure (typeOf prop)
        Nothing -> pure TUnit

-- | Try to get the constant value of an identifier.
asConstant :: (Monad m) => Ident -> IdentEnvT m (Maybe Literal)
asConstant ident = do
    found <- searchProp ident
    case found of
        Just prop -> pure (constant prop)
        Nothing -> pure Nothing

-- | Get the state of an identifier.
identState :: (Monad m) => Ident -> IdentEnvT m TypedState
identState ident = do
    ty <- getTyOf ident
    pure $ TypedState ty $ identLoc ident

updateProp :: (Monad m) => Ident -> (IdentProp -> IdentProp) -> IdentEnvT m ()
updateProp ident f =
    modify $ \env ->
        env
            { identProps = adjust f ident (identProps env)
            }

genNewVar :: (Monad m) => Ty -> IdentEnvT m Ident
genNewVar ty = do
    newIdent <- gets (CompilerGenerated . nextIdent)
    modify $ \env ->
        env
            { nextIdent = nextIdent env + 1
            }
    _ <- registerProp newIdent (IdentProp ty Nothing False Nothing)
    pure newIdent

loadTypeEnv :: (Monad m) => TypeEnv -> IdentEnvT m ()
loadTypeEnv typeEnv = do
    mapM_
        ( \(ident, tId) ->
            registerProp ident (IdentProp (removeVar typeEnv (TVar tId)) Nothing False Nothing)
        )
        $ toList vars
  where
    vars = variables typeEnv

reportEnv :: (Monad m) => IdentEnvT m [Text]
reportEnv =
    gets
        ( map
            ( \(ident, IdentProp ty c isC _) ->
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
            . toList
            . identProps
        )
