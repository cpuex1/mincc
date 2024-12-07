module Globals (extractGlobals) where

import IdentAnalysis (IdentEnvT)
import Syntax (KExpr)

extractGlobals :: KExpr -> IdentEnvT m ()
extractGlobals = undefined
