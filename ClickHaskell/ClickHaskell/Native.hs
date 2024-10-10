{-# LANGUAGE 
    OverloadedStrings
  , DeriveAnyClass
  , NamedFieldPuns
  , NumericUnderscores
#-}
module ClickHaskell.Native where

-- Internal dependencies
import ClickHaskell.Native.Packets (sendHelloPacket, sendQueryPacket, ChCredential(..), sendPingPacket, sendDataPacket)

-- GHC included
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, throw)
import Data.Maybe (fromMaybe, listToMaybe)
import Network.Socket
import System.Timeout (timeout)

-- External
import Network.Socket.ByteString (recv)


data ConnectionError
  = NoAdressResolved
  | EstablishTimeout
  deriving (Show, Exception)

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
      (Just chHost)
      (Just chPort)

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

dev :: IO ()
dev = do
  (sock, _sockAddr) <- openNativeConnection devCredential
  sendHelloPacket sock devCredential
  print =<< recv sock 4096
  print "Hello packet readed. Sending ping"

  sendPingPacket sock
  print =<< recv sock 4096
  print "Ping packet readed. Sending query"

  sendQueryPacket sock devCredential "SELECT 5;"
  sendPingPacket sock
  sendDataPacket sock
  print =<< recv sock 4096

devCredential :: ChCredential
devCredential = MkChCredential
  { chLogin = "default"
  , chPass = ""
  , chDatabase = "default"
  , chHost = "localhost"
  , chPort = "9000"
  }
