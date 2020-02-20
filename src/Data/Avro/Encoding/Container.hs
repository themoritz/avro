{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.Encoding.Container
where

import           Data.Avro.Encoding.FromEncoding (getValue)
import           Data.Avro.Encoding.Value        (FromValue (..))
import           Data.Avro.Schema                (Schema)
import qualified Data.Avro.Schema                as Schema
import qualified Data.Avro.Schema.Deconflict     as Schema
import           Data.Binary.Get                 (Get)
import qualified Data.Binary.Get                 as Get
import qualified Data.ByteString.Lazy            as BL

import Data.Avro.Internal.Container (consumeN, decodeRawBlocks)

decodeContainerWithEmbeddedSchema :: forall a. FromValue a => BL.ByteString -> [Either String a]
decodeContainerWithEmbeddedSchema payload =
  case decodeRawBlocks payload of
    Left err                       -> [Left err]
    Right (writerSchema, mbBlocks) -> mbBlocks >>= decodeBlock writerSchema

decodeContainerWithReaderSchema :: forall a. FromValue a => Schema -> BL.ByteString -> [Either String a]
decodeContainerWithReaderSchema readerSchema payload =
  case decodeRawBlocks payload of
    Left err -> [Left err]
    Right (writerSchema, mbBlocks) ->
      case Schema.deconflict writerSchema readerSchema of
        Left err     -> [Left err]
        Right schema -> mbBlocks >>= decodeBlock schema

decodeBlock :: forall a. FromValue a => Schema -> Either String (Int, BL.ByteString) -> [Either String a]
decodeBlock _ (Left err)               = [Left err]
decodeBlock sch (Right (nrObj, bytes)) =
  snd $ consumeN (fromIntegral nrObj) (decodeValue sch) bytes
  where
    decodeValue sch bytes =
      case Get.runGetOrFail (getValue sch) bytes of
        Left (bs', _, err)  -> (bs', Left err)
        Right (bs', _, res) -> (bs', fromValue res)
