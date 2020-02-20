{-# LANGUAGE LambdaCase #-}
module Data.Avro.Encoding.Convert
where

import           Data.Avro.Encoding.Value
import           Data.Avro.Schema         (Schema, fields, fldName)
import qualified Data.Avro.Types.Value    as Old
import qualified Data.HashMap.Strict      as HashMap
import           Data.Vector              as V

-- | This function will be unnecessary when we fully migrate to 'Value'
convertValue :: Old.Value Schema -> Value
convertValue = \case
  Old.Null -> Null
  Old.Boolean v       -> Boolean v
  Old.Int v           -> Int v
  Old.Long v          -> Long v
  Old.Float v         -> Float v
  Old.Double v        -> Double v
  Old.Bytes v         -> Bytes v
  Old.String v        -> String v
  Old.Array v         -> Array $ fmap convertValue v
  Old.Map v           -> Map $ fmap convertValue v
  Old.Fixed _ v       -> Fixed v
  Old.Enum _ i v      -> Enum i v
  Old.Union vs sch v  ->
    case V.elemIndex sch vs of
      Just ix -> Union ix (convertValue v)
      Nothing -> error "Union contains a value of an unknown schema"
  Old.Record sch vs   ->
    let
      fldNames = fldName <$> fields sch
      values = fmap (\n -> convertValue $ vs HashMap.! n) fldNames
    in Record $ V.fromList values
