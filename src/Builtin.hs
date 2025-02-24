{-# LANGUAGE OverloadedStrings #-}

module Builtin (
    BuiltinFunction (..),
    builtinFunctions,
    findBuiltin,
    builtinMakeTuple,
) where

import Data.Text (Text)
import Syntax (Ident (ExternalIdent))
import Typing (Ty, TypeKind (TFloat, TFun, TInt))

data BuiltinFunction
    = BuiltinFunction
    { builtinName :: Ident
    , builtinType :: Ty
    , builtinInst :: Text
    }
    deriving (Show, Eq)

builtinFunctions :: [BuiltinFunction]
builtinFunctions =
    [ BuiltinFunction (ExternalIdent "float_of_int") (TFun [TInt] TFloat) "fitf"
    , BuiltinFunction (ExternalIdent "int_of_float") (TFun [TFloat] TInt) "ffti"
    , BuiltinFunction (ExternalIdent "sqrt") (TFun [TFloat] TFloat) "fsqrt"
    , BuiltinFunction (ExternalIdent "inv") (TFun [TFloat] TFloat) "finv"
    , BuiltinFunction (ExternalIdent "fneg") (TFun [TFloat] TFloat) "fneg"
    , BuiltinFunction (ExternalIdent "fsqr") (TFun [TFloat] TFloat) "fsqr"
    , BuiltinFunction (ExternalIdent "fabs") (TFun [TFloat] TFloat) "fabs"
    , BuiltinFunction (ExternalIdent "fhalf") (TFun [TFloat] TFloat) "fhalf"
    , BuiltinFunction (ExternalIdent "floor") (TFun [TFloat] TFloat) "ffloor"
    ]

findBuiltin :: Ident -> Maybe BuiltinFunction
findBuiltin ident = lookup ident $ map (\b -> (builtinName b, b)) builtinFunctions

builtinMakeTuple :: Ident
builtinMakeTuple = ExternalIdent "builtin_mk_tuple"
