{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

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
import Data.Word (Word16, Word32)

-- Amiga Disk Format (ADF)
-- http://lclevy.free.fr/adflib/adf_info.html

main :: IO ()
main = do
    blocks <- blockify <$> BS.readFile "test.adf"
    let result = parseOnly bootBlockP (head blocks)
    print result

    let result2 = unwrap $ parseOnly rootBlockP (blocks !! 880)
    print result2

    let fileBlocks = map (\x -> blocks !! fromIntegral x) $ filter (/= 0) result2.rbHashTable

    let result3 = fmap (unwrap . parseOnly blockTypeP) fileBlocks
    print result3

-- contents :: [ByteString] -> [Word32] -> [ByteString]
-- contents blocks hashTable =
--     fmap (parseOnly Block) $ fmap (blocks !!) $ filter (/=0) hashTable

unwrap :: Either a b -> b
unwrap x = case x of
    Right b -> b
    Left _ -> error "not Right"

blockTypeP :: Parser (Word32, Word32)
blockTypeP = do
    _type <- ulong
    unusedUlong (128 - 2)
    secType <- ulong
    pure (_type, secType)

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

cBSIZE :: Int
cBSIZE = 512
cROOTBLOCK_DD :: Int
cROOTBLOCK_DD = 880
cROOTBLOCK_HD :: Int
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

stringP :: Int -> Parser String
stringP maxlen = do
    len <- anyWord8
    name <- replicateM (fromIntegral len) anyChar

    replicateM_ (maxlen - fromIntegral len) anyWord8

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
        <*> stringP 30
        <* anyWord8 -- UNUSED 1 char,
        <* (anyWord32be >> anyWord32be) -- UNUSED 2 ulong,
        <*> diskDateP
        <*> diskDateP
        <*> anyWord32be
        <*> anyWord32be
        <*> anyWord32be
        <*> anyWord32be

data DirectoryBlock = DirectoryBlock
    { hashTable :: [Word32]
    , uid :: Word16
    , gid :: Word16
    , dirname :: String
    , nextLink :: Word32
    , hashChain :: Word32
    , parent :: Word32
    }
    deriving (Show)

{-
\* User directory block (BSIZE bytes)
------------------------------------------------------------------------------------------------
        0/ 0x00	ulong	1	type		block primary type = T_HEADER (value 2)
        4/ 0x04	ulong	1	header_key	self pointer
	8/ 0x08	ulong 	3 	UNUSED		unused (== 0)
       20/ 0x14	ulong	1	chksum		normal checksum algorithm
       24/ 0x18	ulong	*	ht[]		hash table (entry block number)
        	                                * = (BSIZE/4) - 56
                	                        for floppy disk: size= 72 longwords
BSIZE-200/-0xc8	ulong	2	UNUSED		unused (== 0)
BSIZE-196/-0xc8	ushort	1 	UID 		User ID
BSIZE-194/-0xc8	ulong	1	GID		Group ID
BSIZE-192/-0xc0	ulong	1	protect		protection flags (set to 0 by default)

                                        Bit     If set, means

                                           If MultiUser FileSystem : Owner
					0	delete forbidden (D)
					1	not executable (E)
					2	not writable (W)
					3	not readable (R)

					4	is archived (A)
					5	pure (reetrant safe), can be made resident (P)
					6	file is a script (Arexx or Shell) (S)
					7	Hold bit. if H+P (and R+E) are set the file
                                                 can be made resident on first load (OS 2.x and 3.0)

                                        8       Group (D) : is delete protected
                                        9       Group (E) : is executable
                                       10       Group (W) : is writable
                                       11       Group (R) : is readable

                                       12       Other (D) : is delete protected
                                       13       Other (E) : is executable
                                       14       Other (W) : is writable
                                       15       Other (R) : is readable
                                    30-16	reserved
				       31	SUID, MultiUserFS Only

BSIZE-188/-0xbc	ulong	1	UNUSED		unused (== 0)
BSIZE-184/-0xb8	char	1	comm_len	directory comment length
BSIZE-183/-0xb7	char	79	comment[]	comment (max. 79 chars permitted)
BSIZE-104/-0x69	char	12	UNUSED		set to 0
BSIZE- 92/-0x5c	ulong	1	days		last access date (days since 1 jan 78)
BSIZE- 88/-0x58	ulong	1	mins		last access time
BSIZE- 84/-0x54	ulong	1	ticks		in 1/50s of a seconds
BSIZE- 80/-0x50	char	1	name_len	directory name length
BSIZE- 79/-0x4f char	30	dirname[]	directory (max. 30 chars permitted)
BSIZE- 49/-0x31 char	1	UNUSED		set to 0
BSIZE- 48/-0x30 ulong	2	UNUSED		set to 0
BSIZE- 40/-0x28	ulong	1	next_link	FFS : hardlinks chained list (first=newest)
BSIZE- 36/-0x24	ulong	5	UNUSED		set to 0
BSIZE- 16/-0x10	ulong	1	hash_chain	next entry ptr with same hash
BSIZE- 12/-0x0c	ulong	1	parent		parent directory
BSIZE-  8/-0x08	ulong	1	extension	FFS : first directory cache block
BSIZE-  4/-0x04	ulong	1	sec_type	secondary type : ST_USERDIR (== 2)
------------------------------------------------------------------------------------------------
-}

ulong :: Parser Word32
ulong = anyWord32be

ushort :: Parser Word16
ushort = anyWord16be

unusedUlong :: Int -> Parser ()
unusedUlong n = replicateM_ n ulong

unusedChar :: Int -> Parser ()
unusedChar n = replicateM_ n anyWord8

directoryBlockP :: Parser DirectoryBlock
directoryBlockP = do
    _ <- word32be 2 -- T_HEADER = 2
    _headerKey <- ulong
    unusedUlong 3
    _checksum <- ulong
    hashTable <- replicateM ((cBSIZE `div` 4) - 56) ulong
    unusedUlong 2
    uid <- ushort
    gid <- ushort --- TODO ulong eller ushort????
    _protect <- ulong
    unusedUlong 1
    _comment <- stringP 79
    unusedChar 12
    _lastAccess <- diskDateP
    dirName <- stringP 30
    unusedChar 1
    unusedUlong 2
    nextLink <- ulong
    unusedUlong 5
    hashChain <- ulong
    parent <- ulong
    _extension <- ulong
    _secType <- ulong

    pure $
        DirectoryBlock
            { hashTable = hashTable
            , uid = uid
            , gid = gid
            , dirname = dirName
            , nextLink = nextLink
            , hashChain = hashChain
            , parent = parent
            }

data FileHeaderBlock = FileHeaderBlock
    { highSeq :: Word32
    , firstData :: Word32
    , dataBlocks :: [Word32]
    , fileSize :: Word32
    , fileName :: String
    , hashChain :: Word32
    , parent :: Word32
    }
    deriving (Show)

fileHeaderBlockP :: Parser FileHeaderBlock
fileHeaderBlockP = do
    _type <- word32be 2
    _headerKey <- ulong
    highSeq <- ulong
    _dataSize <- ulong
    firstData <- ulong
    _checksum <- ulong
    dataBlocks <- replicateM ((cBSIZE `div` 4) - 56) ulong
    unusedUlong 1
    _uid <- ushort
    _gid <- ushort
    _protect <- ulong
    fileSize <- ulong
    _comment <- stringP 79
    unusedChar 12
    _lastChange <- diskDateP
    fileName <- stringP 30
    unusedChar 1
    unusedUlong 1
    _realEntry <- ulong
    _nextLink <- ulong
    unusedUlong 5
    hashChain <- ulong
    parent <- ulong
    _extension <- ulong
    _secType <- ulong

    pure $
        FileHeaderBlock
            { highSeq = highSeq
            , firstData = firstData
            , dataBlocks = dataBlocks
            , fileSize = fileSize
            , fileName = fileName
            , hashChain = hashChain
            , parent = parent
            }

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
