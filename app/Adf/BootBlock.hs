{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Adf.BootBlock where

import Adf.Parsers (blockPtrP)
import Adf.Types (BlockPtr (..), FileSystem (..), RawBlock, bytes)
import Adf.Util (unwrap)
import Data.Attoparsec.Binary (anyWord32be)
import Data.Attoparsec.ByteString (Parser, anyWord8, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Word (Word32)

data BootBlock = BootBlock
    { fileSystem :: FileSystem
    , checksum :: Word32
    , rootblock :: BlockPtr
    }
    deriving (Show)

parseBoot :: RawBlock -> BootBlock
parseBoot block = unwrap $ parseOnly bootBlockP block.bytes

{-
\* BootBlock
-------------------------------------------------------------------------------
offset  size    number  name            meaning
-------------------------------------------------------------------------------
0/0x00  char    4       DiskType        'D''O''S' + flags
                                        flags = 3 least signifiant bits
                                               set         clr
                                          0    FFS         OFS
                                          1    INTL ONLY   NO_INTL ONLY
                                          2    DIRC&INTL   NO_DIRC&INTL
4/0x04  ulong   1       Chksum          special block checksum
8/0x08  ulong   1       Rootblock       Value is 880 for DD and HD
                                        (yes, the 880 value is strange for HD)
12/0x0c char    *       Bootblock code  (see 5.2 'Bootable disk' for more info)
                                        The size for a floppy disk is 1012,
                                        for a harddisk it is
                                        (DosEnvVec->Bootblocks * BSIZE) - 12
-------------------------------------------------------------------------------
-}
bootBlockP :: Parser BootBlock
bootBlockP = do
    _ <- char 'D' >> char 'O' >> char 'S'
    fs <-
        anyWord8 >>= \case
            0 -> pure OFS
            1 -> pure FFS
            2 -> pure OFS_INTL
            3 -> pure FFS_INTL
            4 -> pure OFS_DIRC_INTL
            5 -> pure FFS_DIRC_INTL
            invalid -> fail $ "Invald flag configuration: " ++ show invalid

    cs <- anyWord32be
    rootblock <- blockPtrP

    pure $
        BootBlock
            { fileSystem = fs
            , checksum = cs
            , rootblock = rootblock
            }
