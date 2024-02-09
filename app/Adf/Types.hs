{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Adf.Types where

import Data.ByteString (ByteString)
import Data.Word (Word32)

newtype BlockPtr = BlockPtr Word32 deriving (Show)

newtype RawBlock = RawBlock {bytes :: ByteString}

data Disk = Disk
    { blocks :: [RawBlock]
    , fileName :: String
    }

data FileSystem
    = OFS
    | FFS
    | OFS_INTL
    | FFS_INTL
    | OFS_DIRC_INTL
    | FFS_DIRC_INTL
    deriving (Show)
