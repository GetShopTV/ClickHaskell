{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, NamedFieldPuns, NumericUnderscores #-} 
module ClickHaskell.Native where

-- Internal dependencies
import ClickHaskell.DbTypes

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Control.Monad (foldM)
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import Data.ByteString.Char8 as BS8 (toStrict, unpack)
import Data.ByteString.Internal (accursedUnutterablePerformIO)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text as Text (Text, unpack)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Bits (..), Ptr, Storable (..), malloc, shiftR)
import GHC.Generics (Generic)
import Network.Socket
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString (recv, send)

openNativeConnection :: ChCredential -> IO (Socket, SockAddr)
openNativeConnection MkChCredential{chHost, chPort} = do
  AddrInfo
    { addrFamily
    , addrSocketType
    , addrProtocol
    , addrAddress
    } <-
      fromMaybe (throw NoAdressResolved)
    . listToMaybe
    <$>
    getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just $ Text.unpack chHost)
      (Just $ Text.unpack chPort)

  (fromMaybe (throw EstablishTimeout) <$>) . timeout 3_000_000 $
    bracketOnError
      (socket addrFamily addrSocketType addrProtocol)
      (\sock ->
        catch @SomeException
          (finally
            (shutdown sock ShutdownBoth)
            (close sock)
          )
          (const $ pure ())
      )
      (\sock -> do
         setSocketOption sock NoDelay 1
         setSocketOption sock KeepAlive 1
         connect sock addrAddress
         pure (sock, addrAddress)
      )

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = "default"
  , chHost = "localhost"
  , chPort = "9000"
  }


data ChCredential = MkChCredential
  { chLogin    :: Text
  , chPass     :: Text
  , chDatabase :: Text
  , chHost     :: Text
  , chPort     :: Text
  }
  deriving (Generic, Show, Eq)

data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)

class
  Deserializable chType
  where
  deserialize :: Ptr Word8 -> chType

class Serializable chType
  where
  serialize :: chType -> Ptr Word8

instance Serializable ChUInt64 where
  serialize = accursedUnutterablePerformIO . asPtrWord8 . fromChType @ChUInt64 @Word64

asPtrWord8 :: (Storable a, Bits a, Integral a) => a -> IO (Ptr Word8)
asPtrWord8 storableBits = do
  pointer <- malloc
  foldM
    (\ptr index -> do
      pokeElemOff
        ptr
        index
        (fromIntegral $ storableBits `shiftR` (8 * index))
      pure ptr
    )
    pointer
    [0 .. sizeOf storableBits - 1]

dev :: IO ()
dev = do
  (sock, _sockAddr) <- openNativeConnection devCredential
  sendHello sock


{-# SPECIALIZE leb128 :: Word8 -> Builder #-}
{-# SPECIALIZE leb128 :: Word16 -> Builder #-}
{-# SPECIALIZE leb128 :: Word32 -> Builder #-}
{-# SPECIALIZE leb128 :: Word64 -> Builder #-}
leb128 :: (Bits a, Num a, Integral a) => a -> Builder
leb128 = go
  where
    go i
      | i <= 127
      = word8 (fromIntegral i :: Word8)
      | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        word8 (setBit (fromIntegral i) 7) <> go (i `unsafeShiftR` 7)


sendHello :: Socket -> IO ()
sendHello sock = do
  _sentSize <- send sock
    (toStrict . toLazyByteString . mconcat $
      [ leb128 @Word8 0            -- Hello packet code
      , leb128 @Word8 5, "hello"   -- Client name: "Hello"
      , leb128 @Word8 0            -- Major version: 0
      , leb128 @Word8 1            -- Minor version: 0
      , leb128 @Word16 55_255      -- Protocol version
      , leb128 @Word8 7, "default" -- Database name: "default"
      , leb128 @Word8 7, "default" -- User name: "default"
      , leb128 @Word8 0, ""        -- Password: ""
      ]
    )
  bs <- recv sock 1  
  case bs of
    "\NUL" -> print bs
    _ -> error $ "Got unknown packet code: " <> BS8.unpack bs  
