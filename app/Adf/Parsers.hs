module Adf.Parsers where

import Adf.Types
import Control.Monad (replicateM_)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be)
import Data.Attoparsec.ByteString (Parser, anyWord8)
import Data.Word (Word16, Word32)

ulong :: Parser Word32
ulong = anyWord32be

ushort :: Parser Word16
ushort = anyWord16be

unusedUlong :: Int -> Parser ()
unusedUlong n = replicateM_ n ulong

unusedChar :: Int -> Parser ()
unusedChar n = replicateM_ n anyWord8

blockPtrP :: Parser BlockPtr
blockPtrP = BlockPtr <$> ulong

maybeBlockPtrP :: Parser (Maybe BlockPtr)
maybeBlockPtrP = do
    ptr <- ulong
    pure $ case ptr of
        0 -> Nothing
        _ -> Just (BlockPtr ptr)
