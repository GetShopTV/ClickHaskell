{-#LANGUAGE
    DataKinds
  , AllowAmbiguousTypes
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}

module IntegrationTests.WriteReadEquality
  ( runWriteReadEqualityTest
  ) where

-- Internal
import ClickHaskell.Client (WritableInto, ReadableFrom, ChCredential(..), insertInto, runStatement, selectFrom)
import ClickHaskell.Tables (Table, Column)
import ClickHaskell.DbTypes
  ( toChType
  , ChInt8, ChInt16, ChInt32, ChInt64, ChInt128
  , ChUInt8, ChUInt16, ChUInt32, ChUInt64, ChUInt128
  , ChUUID, ChDateTime, ChString, Int128, Word128
  , Nullable
  )


-- External
import Network.HTTP.Client as H (newManager, defaultManagerSettings, Manager)


-- GHC included
import Control.Exception      (bracket)
import Control.Monad          (when)
import Data.Int               (Int8, Int16, Int32, Int64)
import Data.Word              (Word8, Word16, Word32, Word64)
import GHC.Generics           (Generic)


runWriteReadEqualityTest :: ChCredential -> IO ()
runWriteReadEqualityTest creds = do
  bracket
    (newManager defaultManagerSettings)
    (\manager -> runStatement manager creds "TRUNCATE writeReadEqualityTable")
    (\manager -> runTest manager creds)

runTest :: Manager -> ChCredential -> IO ()
runTest manager cred = do
  insertInto
    @TestTable
    @TestData
    manager
    cred
    [testData]

  result <-
    selectFrom
      @TestTable
      @TestData
      manager
      cred

  let testLabel = "WriteReadEquality: "
  (when (length result /= 1) . error)
    (  testLabel
    <> "Expected single result from reading. "
    <> "But got: " <> show (length result) <> ".")

  (when (head result /= testData) . error)
    (  testLabel <> "Unequal result.\n"
    <> "Writed data: " <> show testData <> "\n"
    <> "Readed data: " <> show (head result))

  print $ testLabel <> "Ok"


type TestTable = Table "writeReadEqualityTable"
  '[ Column "dateTime" ChDateTime
   , Column "dateTimeNullable" (Nullable ChDateTime)
   , Column "int128" ChInt128
   , Column "int128Nullable" (Nullable ChInt128)
   , Column "int16" ChInt16
   , Column "int16Nullable" (Nullable ChInt16)
   , Column "int32" ChInt32
   , Column "int32Nullable" (Nullable ChInt32)
   , Column "int64" ChInt64
   , Column "int64Nullable" (Nullable ChInt64)
   , Column "int8" ChInt8
   , Column "int8Nullable" (Nullable ChInt8)
   , Column "string" ChString
   , Column "stringNullable" (Nullable ChString)
   , Column "uint128" ChUInt128
   , Column "uint128Nullable" (Nullable ChUInt128)
   , Column "uint16" ChUInt16
   , Column "uint16Nullable" (Nullable ChUInt16)
   , Column "uint32" ChUInt32
   , Column "uint32Nullable" (Nullable ChUInt32)
   , Column "uint64" ChUInt64
   , Column "uint64Nullable" (Nullable ChUInt64)
   , Column "uint8" ChUInt8
   , Column "uint8Nullable" (Nullable ChUInt8)
   , Column "uuid" ChUUID
   , Column "uuidNullable" (Nullable ChUUID)
   ]

data TestData = MkTestData
  { dateTime :: ChDateTime
  , dateTimeNullable :: Nullable ChDateTime
  , int128 :: ChInt128
  , int128Nullable :: Nullable ChInt128
  , int16 :: ChInt16
  , int16Nullable :: Nullable ChInt16
  , int32 :: ChInt32
  , int32Nullable :: Nullable ChInt32
  , int64 :: ChInt64
  , int64Nullable :: Nullable ChInt64
  , int8 :: ChInt8
  , int8Nullable :: Nullable ChInt8
  , string :: ChString
  , stringNullable :: Nullable ChString
  , uint128 :: ChUInt128
  , uint128Nullable :: Nullable ChUInt128
  , uint16 :: ChUInt16
  , uint16Nullable :: Nullable ChUInt16
  , uint32 :: ChUInt32
  , uint32Nullable :: Nullable ChUInt32
  , uint64 :: ChUInt64
  , uint64Nullable :: Nullable ChUInt64
  , uint8 :: ChUInt8
  , uint8Nullable :: Nullable ChUInt8
  , uuid :: ChUUID
  , uuidNullable :: Nullable ChUUID
  }
  deriving (Generic, Show, Eq)

instance ReadableFrom TestTable TestData
instance WritableInto TestTable TestData

testData :: TestData
testData = MkTestData
  { dateTime = toChType (0 :: Word32)
  , dateTimeNullable = Nothing
  , int128 = toChType (-128 :: Int128)
  , int128Nullable = toChType $ Just (-128 :: Int128)
  , int16 = toChType (-16 :: Int16)
  , int16Nullable = toChType $ Just (-16 :: Int16)
  , int32 = toChType (-32 :: Int32)
  , int32Nullable = toChType $ Just (-32 :: Int32)
  , int64 = toChType (-64 :: Int64)
  , int64Nullable = toChType $ Just (-64 :: Int64)
  , int8 = toChType (-8 :: Int8)
  , int8Nullable = toChType $ Just (-8 :: Int8)
  , string = "string"
  , stringNullable = Just "string"
  , uint128 = toChType (128 :: Word128)
  , uint128Nullable = toChType $ Just (128 :: Word128)
  , uint16 = toChType (16 :: Word16)
  , uint16Nullable = toChType $ Just (16 :: Word16)
  , uint32 = toChType (32 :: Word32)
  , uint32Nullable = toChType $ Just (32 :: Word32)
  , uint64 = toChType (64 :: Word64)
  , uint64Nullable = toChType $ Just (64 :: Word64)
  , uint8 = toChType (8 :: Word8)
  , uint8Nullable = toChType $ Just (8 :: Word8)
  , uuid = toChType (123456789 :: Word64)
  , uuidNullable = Nothing
  }
