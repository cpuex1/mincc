{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Globals (defaultGlobalTable, extractGlobals, reportGlobals) where

import Control.Monad.State (MonadTrans (lift), StateT, gets, modify)
import Data.Text (Text, pack)
import Display (Display, display)
import IdentAnalysis (IdentEnvT, IdentProp (IdentProp), getTyOf, registerProp, searchProp)
import Syntax (Expr (ArrayCreate, If, Let, Tuple), Ident (ExternalIdent), KExpr, Literal (LInt), Pattern (PVar), subst)
import Typing (Ty)

data GlobalConstant
    = GLiteral Literal
    | GExternal Text
    deriving (Show, Eq)

data GlobalKind
    = GTuple [GlobalConstant]
    | GArray Int GlobalConstant
    deriving (Show, Eq)

data GlobalProp = GlobalProp
    { globalName :: Text
    , globalType :: Ty
    , globalValue :: GlobalKind
    , globalOffset :: Int
    }
    deriving (Show, Eq)

globalSize :: GlobalKind -> Int
globalSize (GTuple globals) = length globals
globalSize (GArray size _) = size

data GlobalTable = GlobalTable
    { globalTable :: [(Text, GlobalProp)]
    , totalSize :: Int
    }
    deriving (Show, Eq)

type GlobalState m = StateT GlobalTable (IdentEnvT m)

defaultGlobalTable :: GlobalTable
defaultGlobalTable = GlobalTable [] 0

addGlobalTable :: (Monad m) => Text -> Ty -> GlobalKind -> GlobalState m ()
addGlobalTable name ty kind = do
    offset <- gets totalSize
    let prop = GlobalProp name ty kind offset
    let size = globalSize $ globalValue prop
    modify $ \table ->
        table
            { globalTable = globalTable table ++ [(name, prop)]
            , totalSize = totalSize table + size
            }

asGlobal :: (Monad m) => Ident -> IdentEnvT m (Maybe GlobalConstant)
asGlobal (ExternalIdent ext) = pure $ Just $ GExternal ext
asGlobal ident = do
    prop <- searchProp ident
    case prop of
        Just (IdentProp _ (Just lit) _) ->
            pure $ Just $ GLiteral lit
        _ -> pure Nothing

extractGlobals :: (Monad m) => KExpr -> GlobalState m KExpr
extractGlobals (Let state1 (PVar v) (Tuple state2 values) body) = do
    globals <- sequence <$> (lift . mapM asGlobal) values
    case globals of
        Just globals' -> do
            -- Register it to the global table.
            let name = display v
            ty <- lift $ getTyOf v
            addGlobalTable name ty $ GTuple globals'

            -- Replace it with an external identifier.
            let newV = ExternalIdent name
            lift $ registerProp newV $ IdentProp ty Nothing False
            let body' = subst v newV v newV body
            extractGlobals body'
        Nothing -> do
            body' <- extractGlobals body
            pure $ Let state1 (PVar v) (Tuple state2 values) body'
extractGlobals (Let state1 (PVar v) (ArrayCreate state2 idx val) body) = do
    idxGlobal <- lift $ asGlobal idx
    valGlobal <- lift $ asGlobal val
    case (idxGlobal, valGlobal) of
        (Just (GLiteral (LInt idxV)), Just valGlobal') -> do
            -- Register it to the global table.
            let name = display v
            ty <- lift $ getTyOf v
            addGlobalTable name ty $ GArray idxV valGlobal'

            -- Replace it with an external identifier.
            let newV = ExternalIdent name
            lift $ registerProp newV $ IdentProp ty Nothing False
            let body' = subst v newV v newV body
            extractGlobals body'
        _ -> do
            body' <- extractGlobals body
            pure $ Let state1 (PVar v) (ArrayCreate state2 idx val) body'
extractGlobals (Let state pattern expr body) = do
    -- Do not search for globals in the expression.
    body' <- extractGlobals body
    pure $ Let state pattern expr body'
extractGlobals (If state cond then' else') = do
    then'' <- extractGlobals then'
    else'' <- extractGlobals else'
    pure $ If state cond then'' else''
extractGlobals expr = pure expr

-- | Report all globals with pretty printing.
reportGlobals :: (Monad m) => GlobalState m [Text]
reportGlobals = do
    table <- gets (map snd . globalTable)
    pure $ map display table

instance Display GlobalProp where
    display prop =
        globalName prop
            <> " : "
            <> display (globalType prop)
            <> " (offset: "
            <> pack (show (globalOffset prop))
            <> ", size: "
            <> pack (show (globalSize $ globalValue prop))
            <> ")"
