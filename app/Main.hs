{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString qualified as BS

import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString (Parser, anyWord8, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Word (Word32)

-- Amiga Disk Format (ADF)
-- http://lclevy.free.fr/adflib/adf_info.html

main :: IO ()
main = do
    bytes <- BS.readFile "test.adf"

    let result = parseOnly bootBlockP bytes
    print result

{-
Vocabulary

A 'word' or 'short' is a 2-byte (16 bits) integer, a 'long' a 4-byte (32 bits) integer. Values are unsigned unless otherwise noted.

A 'block' in this document will be 512 consecutive bytes on disk, unless noted otherwise, the variable 'BSIZE' will denote the blocksize.
The word 'sector' and 'block' will be used as synonyms here, even if 'sector' is usually related to the physical side, and the 'block' to
the logical side. This is because the AmigaDOS can only handle one sector per block. Some other Unix filesystems can have more then one sector per block.

A block pointer is the number of this block on the disk. The first one is the #0 block.
There are 'logical' and 'physical' block pointers. 'Logical' ones are related to the start of one volume, 'physical' one are related
to the start of a physical media. If a volume starts at the #0 physical sector, a physical pointer and a logical pointer is the same thing,
like with floppies.
-}

cBSIZE :: Word32
cBSIZE = 512
cROOTBLOCK_DD :: Word32
cROOTBLOCK_DD = 880
cROOTBLOCK_HD :: Word32
cROOTBLOCK_HD = 1760

data FileSystem = OFS | FFS | OFS_INTL | FFS_INTL | OFS_DIRC_INTL | FFS_DIRC_INTL deriving (Show)
data BootBlock = BootBlock
    { bbFileSystem :: FileSystem
    , bbChecksum :: Word32
    , bbRootblock :: Word32
    }
    deriving (Show)

data RootBlock = RootBlock
    { rbType :: Word32
    , rbDiskname :: String
    }
    deriving (Show)

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
    rootblock <- anyWord32be

    pure $
        BootBlock
            { bbFileSystem = fs
            , bbChecksum = cs
            , bbRootblock = rootblock
            }
