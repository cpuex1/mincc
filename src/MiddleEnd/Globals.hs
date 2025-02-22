{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module MiddleEnd.Globals (
    searchGlobalTable,
    defaultGlobalTable,
    extractGlobals,
    reportGlobals,
    GlobalTable (..),
    GlobalProp (..),
) where

import Builtin (builtinMakeTuple)
import Control.Monad.State (MonadTrans (lift), StateT, gets, modify)
import Data.Map (Map, elems, empty, insert, lookup)
import Data.Text (Text, pack)
import Display (Display, display)
import MiddleEnd.Analysis.Identifier (IdentEnvT, IdentProp (IdentProp), asConstant, getTyOf, registerProp)
import Syntax (Expr (App, ArrayCreate, If, Let, Loop, Tuple), Ident (ExternalIdent), KExpr, Literal (LInt), Pattern (PUnit, PVar), TypedState (TypedState), dummyLoc, subst)
import Typing (Ty, TypeKind (TUnit))
import Prelude hiding (lookup)

data GlobalProp = GlobalProp
    { globalName :: Text
    , globalType :: Ty
    , globalSize :: Int
    , globalOffset :: Int
    }
    deriving (Show, Eq)

data GlobalTable = GlobalTable
    { globalTable :: Map Text GlobalProp
    , startAddr :: Int
    , endAddr :: Int
    }
    deriving (Show, Eq)

type GlobalState m = StateT GlobalTable (IdentEnvT m)

-- | The address of starting the global table.
startGlobalTableAddr :: Int
startGlobalTableAddr = 0x40

defaultGlobalTable :: GlobalTable
defaultGlobalTable = GlobalTable empty startGlobalTableAddr startGlobalTableAddr

addGlobalTable :: (Monad m) => Text -> Ty -> Int -> GlobalState m ()
addGlobalTable name ty size = do
    offset <- gets endAddr
    let prop = GlobalProp name ty size offset
    modify $ \table ->
        table
            { globalTable = insert name prop $ globalTable table
            , endAddr = endAddr table + size
            }

searchGlobalTable :: GlobalTable -> Text -> Maybe GlobalProp
searchGlobalTable table name =
    lookup name $ globalTable table

extractGlobals :: (Monad m) => KExpr -> GlobalState m KExpr
extractGlobals (Let state1 (PVar v) (ArrayCreate state2 idx initVal) body) = do
    -- Globalize an array outside of let-expressions.
    constIdx <- lift $ asConstant idx
    case constIdx of
        Just (LInt constIdx') -> do
            -- Register it to the global table.
            let name = display v
            ty <- lift $ getTyOf v
            addGlobalTable name ty constIdx'

            -- Replace it with an external identifier.
            let newV = ExternalIdent name
            lift $ registerProp newV $ IdentProp ty Nothing False Nothing
            let body' = subst v newV body
            extractGlobals body'
        _ -> do
            -- The size of it cannot be determined at compile time.
            body' <- extractGlobals body
            pure $ Let state1 (PVar v) (ArrayCreate state2 idx initVal) body'
extractGlobals (Let state1 (PVar v) (Tuple _ values) body) = do
    -- Register it to the global table.
    let name = display v
    ty <- lift $ getTyOf v
    addGlobalTable name ty (length values)

    -- Substitute a global variable for a tuple inside bodies.
    let newV = ExternalIdent name
    lift $ registerProp newV $ IdentProp ty Nothing False Nothing
    let body' = subst v newV body

    -- Replace it with builtin_mk_tuple.
    let makingTuple = App (TypedState TUnit dummyLoc) builtinMakeTuple (newV : values)
    extractGlobals $ Let state1 PUnit makingTuple body'
extractGlobals (Let state pattern expr body) = do
    -- Do not search for globals in the expression of let.
    body' <- extractGlobals body
    pure $ Let state pattern expr body'
extractGlobals (If state cond then' else') = do
    then'' <- extractGlobals then'
    else'' <- extractGlobals else'
    pure $ If state cond then'' else''
extractGlobals (Loop state args values body) = do
    body' <- extractGlobals body
    pure $ Loop state args values body'
extractGlobals expr = pure expr

-- | Report all globals with pretty printing.
reportGlobals :: (Monad m) => GlobalState m [Text]
reportGlobals = do
    table <- gets (elems . globalTable)
    pure $ map display table

instance Display GlobalProp where
    display prop =
        globalName prop
            <> " : "
            <> display (globalType prop)
            <> " (offset: "
            <> pack (show (globalOffset prop))
            <> ", size: "
            <> pack (show (globalSize prop))
            <> ")"
