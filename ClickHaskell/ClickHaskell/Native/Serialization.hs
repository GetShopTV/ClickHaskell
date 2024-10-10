module ClickHaskell.Native.Serialization where

-- Internal dependencies
import ClickHaskell.DbTypes

-- GHC included
import Data.ByteString as BS (length)
import Data.ByteString.Builder as BS (Builder, byteString, int16LE, int32LE, int64LE, int8, word64LE, word8)
import Data.ByteString.Char8 as BS8 (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Bits (Bits (..))
import Data.WideWord (Int128(..))
import Foreign (Ptr)

{-# SPECIALIZE vlq128 :: ChUInt8 -> Builder #-}
{-# SPECIALIZE vlq128 :: ChUInt16 -> Builder #-}
{-# SPECIALIZE vlq128 :: ChUInt32 -> Builder #-}
{-# SPECIALIZE vlq128 :: ChUInt64 -> Builder #-}
vlq128 :: (Bits a, Num a, Integral a) => a -> Builder
vlq128 = go
  where
  go i
    | i <= 127  = word8 (fromIntegral i :: Word8)
    | otherwise = word8 (0x80 .&. (fromIntegral i .|. 0x7F)) <> go (i `unsafeShiftR` 7)




class Serializable chType
  where
  serialize :: chType -> Builder

instance Serializable ChUInt8 where
  serialize = word8 . fromChType @ChUInt8 @Word8

instance Serializable ChUInt16 where
  serialize = vlq128 . fromChType @ChUInt16 @Word16

instance Serializable ChUInt32 where
  serialize = vlq128 . fromChType @ChUInt32 @Word32

instance Serializable ChUInt64 where
  serialize = vlq128 . fromChType @ChUInt64 @Word64

instance Serializable ChUInt128 where
  serialize = vlq128 . fromChType @ChUInt128 @Word128

instance Serializable ChInt8 where
  serialize = int8 . fromChType @ChInt8 @Int8

instance Serializable ChInt16 where
  serialize = int16LE . fromChType @ChInt16 @Int16

instance Serializable ChInt32 where
  serialize = int32LE . fromChType @ChInt32 @Int32

instance Serializable ChInt64 where
  serialize = int64LE . fromChType @ChInt64 @Int64

instance Serializable ChInt128 where
  serialize = (\w128 -> word64LE (int128Hi64 w128) <> word64LE (int128Lo64 w128)) . fromChType @ChInt128 @Int128

instance Serializable ChString where
  serialize str
    =  (serialize @ChUInt64 . fromIntegral . BS.length . fromChType @_ @ByteString) str
    <> (BS.byteString . fromChType @_ @ByteString) str




class
  Deserializable chType
  where
  deserialize :: Ptr Word8 -> chType
