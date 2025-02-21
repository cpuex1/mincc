{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Display (
    insertIndent,
    display,
    Display,
    displayI,
    DisplayI,
) where

import Data.Text (Text, pack, replicate)
import Prelude hiding (replicate)

insertIndent :: Int -> Text
insertIndent depth = replicate depth "    "

class DisplayI a where
    displayI :: Int -> a -> Text

instance Display Int where
    display = pack . show

class Display a where
    display :: a -> Text
