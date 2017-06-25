{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Hiper.Instances () where

import Data.Text.Encoding (encodeUtf8)
import Data.Hiper.Types.Internal
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.C.Types (CDouble, CFloat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Scientific as Scientific

instance  Configurable Value where
  fromValue = Just
  toValue = Just

instance Configurable a => Configurable [a] where
  fromValue = fromValueList
  toValue = toValueList

instance Configurable Bool where
  fromValue (Bool v) = Just v
  fromValue _ = Nothing

  toValue x = Just (Bool x)

convertNumberToNum :: (Num a) => Value -> Maybe a
convertNumberToNum (Number r) =
  case Scientific.floatingOrInteger r of
    (Right i) -> Just $ fromIntegral i
    _ -> Nothing
convertNumberToNum _ = Nothing


convertIntegralToNumber :: (Integral a) => a -> Maybe Value
convertIntegralToNumber n = Just $ Number $ fromIntegral n


instance Configurable Int where
  fromValue = convertNumberToNum
  toValue = convertIntegralToNumber

instance Configurable Integer where
  fromValue = convertNumberToNum
  toValue = convertIntegralToNumber

instance Configurable Int8 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Int16 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Int32 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Int64 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Word where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Word8 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Word16 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Word32 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Configurable Word64 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

convertNumberToFractional :: (RealFloat a) => Value -> Maybe a
convertNumberToFractional (Number r) =
    case Scientific.floatingOrInteger r of
      (Left v) -> Just v
      _ -> Nothing
convertNumberToFractional _ = Nothing

convertFractionalToNumber :: (RealFloat a) => a -> Maybe Value
convertFractionalToNumber f =
  let (c, e) = decodeFloat f
  in Just $ Number $ fromInteger c * 10 ^^ e


instance Configurable Double where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

instance Configurable Float where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

instance Configurable CDouble where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

instance Configurable CFloat where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

-- instance Integral a => Configurable (Ratio a) where
--     convert = convertNumberToFractional

-- instance RealFloat a => Configurable (Complex a) where
--     convert = convertNumberToFractional

-- instance HasResolution a => Configurable (Fixed a) where
--     convert = convertNumberToFractional

instance Configurable T.Text where
  fromValue (String t) = Just t
  fromValue _ = Nothing

  toValue t = Just $ String t

instance Configurable Char where
  fromValue (String t) | T.length t == 1 = Just $ T.head t
  fromValue _ = Nothing

  toValue t = Just $ String $ T.pack [t]

instance Configurable L.Text where
  fromValue = fmap L.fromStrict . fromValue

  toValue lt = Just $ String $ L.toStrict lt

instance Configurable B.ByteString where
  fromValue = fmap encodeUtf8 . fromValue

  toValue bs = Just $ String $ E.decodeUtf8 bs

instance Configurable LB.ByteString where
  fromValue = fmap (LB.fromChunks . (:[])) . fromValue

  toValue lbs = Just $ String $ L.toStrict $ LE.decodeUtf8 lbs
