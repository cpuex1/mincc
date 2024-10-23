{-# LANGUAGE OverloadedStrings #-}

module Path (getExt, changeExt) where

import Data.Text ( splitOn, pack, unpack, intercalate )

getExt :: FilePath -> Maybe String
getExt f = if length sp <= 1 then Nothing else Just $ unpack $ last sp
    where
        sp = splitOn "." $ pack f

changeExt :: String -> FilePath -> FilePath
changeExt ext f = withoutExt ++ "." ++ ext
    where
        sp = splitOn "." $ pack f
        withoutExt = unpack $ intercalate "." $ take (length sp - 1) sp
