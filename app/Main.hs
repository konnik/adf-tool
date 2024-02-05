{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString qualified as BS

import Control.Monad (replicateM, replicateM_)
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString (Parser, anyWord8, parseOnly)
import Data.Attoparsec.ByteString.Char8 (anyChar, char)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Char (ord, toUpper)
import Data.Foldable (foldl')
import Data.Word (Word32)

-- Amiga Disk Format (ADF)
-- http://lclevy.free.fr/adflib/adf_info.html

main :: IO ()
main = do
    blocks <- blockify <$> BS.readFile "test.adf"
    let result = parseOnly bootBlockP (head blocks)
    print result

    let result2 = parseOnly rootBlockP (blocks !! 880)
    print result2

blockify :: ByteString -> [ByteString]
blockify bytes =
    if BS.length bytes == 0
        then []
        else
            let
                (a, rest) = BS.splitAt (fromIntegral cBSIZE) bytes
             in
                a : blockify rest

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

data RootBlock = RootBlock
    { rbType :: Word32
    , rbHeaderKey :: Word32
    , rbHighSeq :: Word32
    , rbHtSize :: Word32
    , rbFirstData :: Word32
    , rbCheckSum :: Word32
    , rbHashTable :: [Word32]
    , rbBmFlag :: Word32
    , rbBmPages :: [Word32]
    , rbBmExt :: Word32
    , rbLastRootAlt :: DiskDate
    , rbDiskname :: String
    , rbLastDiskAlt :: DiskDate
    , rbFsCreation :: DiskDate
    , rbNextHash :: Word32
    , rbParentDir :: Word32
    , rbExtension :: Word32
    , rbSecType :: Word32
    }
    deriving (Show)

data DiskDate = DiskDate Word32 Word32 Word32 deriving (Show)

diskNameP :: Parser String
diskNameP = do
    len <- anyWord8
    name <- replicateM (fromIntegral len) anyChar

    replicateM_ (30 - fromIntegral len) anyWord8

    pure name

diskDateP :: Parser DiskDate
diskDateP =
    DiskDate
        <$> anyWord32be
        <*> anyWord32be
        <*> anyWord32be

rootBlockP :: Parser RootBlock
rootBlockP =
    RootBlock
        <$> anyWord32be
        <*> anyWord32be
        <*> anyWord32be
        <*> anyWord32be
        <*> anyWord32be
        <*> anyWord32be
        <*> replicateM 72 anyWord32be
        <*> anyWord32be
        <*> replicateM 25 anyWord32be
        <*> anyWord32be
        <*> diskDateP
        <*> diskNameP
        <* anyWord8 -- UNUSED 1 char,
        <* (anyWord32be >> anyWord32be) -- UNUSED 2 ulong,
        <*> diskDateP
        <*> diskDateP
        <*> anyWord32be
        <*> anyWord32be
        <*> anyWord32be
        <*> anyWord32be

{-
#include<ctype.h>

int HashName(unsigned char *name)
{
unsigned long hash, l;				/* sizeof(int)>=2 */
int i;

l=hash=strlen(name);
for(i=0; i<l; i++) {
        hash=hash*13;
        hash=hash + toupper(name[i]);	/* not case sensitive */
        hash=hash & 0x7ff;
        }
hash=hash % ((BSIZE/4)-56);		/* 0 < hash < 71
                                         * in the case of 512 byte blocks */

return(hash);
}

-}
hashName :: String -> Word32
hashName name =
    foldl' hash (fromIntegral (length name)) values `mod` 72
  where
    values :: [Word32]
    values = fmap (fromIntegral . ord . toUpper) name -- TODO use safe amiga intl toUpper
    hash :: Word32 -> Word32 -> Word32
    hash acc c = (acc * 13 + c) .&. 0x7ff
