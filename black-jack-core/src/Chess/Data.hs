{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeApplications #-}

-- | Conversion functions
module Chess.Data where

import Cardano.Crypto.Hash (Blake2b_256, hashToBytes, hashWith)
import Codec.Serialise (serialise)
import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Scientific as Scientific
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector as Vector
import PlutusLedgerApi.V2 (
  Data (..),
  ToData,
  toData, UnsafeFromData (unsafeFromBuiltinData), BuiltinData (..),
 )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, frequency, listOf, scale)
import Test.QuickCheck.Modifiers (getPositive, getSmall)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Bifunctor (Bifunctor (bimap))

datumHashBytes :: (ToData a) => a -> ByteString
datumHashBytes =
  hashToBytes . hashWith @Blake2b_256 datumBytes

datumBytes :: (ToData a) => a -> ByteString
datumBytes =
  LBS.toStrict
    . serialise
    . toData

datumJSON :: (ToData a) => a -> ByteString
datumJSON =
  LBS.toStrict
    . Aeson.encode
    . PlutusData
    . toData

fromJSONDatum :: (ToData a, UnsafeFromData a) => Value -> Either Text a
fromJSONDatum = bimap Text.pack (unsafeFromBuiltinData . BuiltinData) . jsonToData

newtype PlutusData = PlutusData Data
  deriving newtype (Eq, Show)

instance ToJSON PlutusData where
  toJSON (PlutusData datum) = dataToJSON datum

instance FromJSON PlutusData where
  parseJSON value =
    case jsonToData value of
      Left err -> fail err
      Right v -> pure $ PlutusData v

genData :: Gen Data
genData =
  frequency
    [ (4, B . BS.pack <$> arbitrary)
    , (3, I <$> arbitrary)
    , (3, List <$> decreasing (listOf genData))
    , (1, Map <$> decreasing (listOf genPair))
    ,
      ( 3
      , Constr
          <$> (getSmall . getPositive <$> arbitrary)
          <*> decreasing (listOf genData)
      )
    ]
 where
  decreasing = scale (`div` (2 :: Int))

genPair :: Gen (Data, Data)
genPair = (,) <$> genData <*> genData

instance Arbitrary PlutusData where
  arbitrary = PlutusData <$> genData

dataToJSON :: Data -> Aeson.Value
dataToJSON = \case
  Constr n fields ->
    object
      [ "constructor" .= n
      , "fields" .= (dataToJSON <$> fields)
      ]
  Map elems ->
    object
      [ "map"
          .= [ object [("k", dataToJSON k), ("v", dataToJSON v)]
             | (k, v) <- elems
             ]
      ]
  List elems ->
    object ["list" .= (dataToJSON <$> elems)]
  B bytes ->
    object ["bytes" .= Text.decodeUtf8 (Hex.encode bytes)]
  I n ->
    object ["int" .= n]

jsonToData ::
  Value -> Either String Data
jsonToData = \case
  (Object m) ->
    case List.sort $ KeyMap.toList m of
      [("int", Number d)] ->
        case Scientific.floatingOrInteger d :: Either Double Integer of
          Left n -> Left ("Cannot have double value in Data: " <> show n)
          Right n -> Right $ I n
      [("bytes", Aeson.String s)]
        | Right bs <- Hex.decode (Text.encodeUtf8 s) ->
            Right $ B bs
      [("list", Aeson.Array vs)] ->
        fmap List $
          traverse jsonToData $
            Vector.toList vs
      [("map", Aeson.Array kvs)] ->
        fmap Map
          . traverse convertKeyValuePair
          $ Vector.toList kvs
      [("constructor", Number d), ("fields", Array vs)] ->
        case Scientific.floatingOrInteger d :: Either Double Integer of
          Left n -> Left ("Cannot have double value in Data: " <> show n)
          Right n ->
            fmap (Constr n)
              . traverse jsonToData
              $ Vector.toList vs
      other -> Left $ "Unexpected object: " <> LT.unpack (LT.decodeUtf8 (Aeson.encode other))
  other -> Left $ "Unexpected JSON " <> LT.unpack (LT.decodeUtf8 (Aeson.encode other))
 where
  convertKeyValuePair ::
    Aeson.Value -> Either String (Data, Data)
  convertKeyValuePair = \case
    (Object m)
      | KeyMap.size m == 2
      , Just k <- KeyMap.lookup "k" m
      , Just v <- KeyMap.lookup "v" m ->
          (,) <$> jsonToData k <*> jsonToData v
    other -> Left $ "Invalid key-value pair " <> LT.unpack (LT.decodeUtf8 (Aeson.encode other))
