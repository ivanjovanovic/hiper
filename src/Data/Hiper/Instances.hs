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

instance  Convertible Value where
  fromValue = Just
  toValue = Just

instance Convertible a => Convertible [a] where
  fromValue = fromValueList
  toValue = toValueList

instance Convertible Bool where
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


instance Convertible Int where
  fromValue = convertNumberToNum
  toValue = convertIntegralToNumber

instance Convertible Integer where
  fromValue = convertNumberToNum
  toValue = convertIntegralToNumber

instance Convertible Int8 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Int16 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Int32 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Int64 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Word where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Word8 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Word16 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Word32 where
    fromValue = convertNumberToNum
    toValue = convertIntegralToNumber

instance Convertible Word64 where
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


instance Convertible Double where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

instance Convertible Float where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

instance Convertible CDouble where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

instance Convertible CFloat where
    fromValue = convertNumberToFractional
    toValue = convertFractionalToNumber

-- instance Integral a => Convertible (Ratio a) where
--     convert = convertNumberToFractional

-- instance RealFloat a => Convertible (Complex a) where
--     convert = convertNumberToFractional

-- instance HasResolution a => Convertible (Fixed a) where
--     convert = convertNumberToFractional

instance Convertible T.Text where
  fromValue (String t) = Just t
  fromValue _ = Nothing

  toValue t = Just $ String t

instance Convertible Char where
  fromValue (String t) | T.length t == 1 = Just $ T.head t
  fromValue _ = Nothing

  toValue t = Just $ String $ T.pack [t]

instance Convertible L.Text where
  fromValue = fmap L.fromStrict . fromValue

  toValue lt = Just $ String $ L.toStrict lt

instance Convertible B.ByteString where
  fromValue = fmap encodeUtf8 . fromValue

  toValue bs = Just $ String $ E.decodeUtf8 bs

instance Convertible LB.ByteString where
  fromValue = fmap (LB.fromChunks . (:[])) . fromValue

  toValue lbs = Just $ String $ L.toStrict $ LE.decodeUtf8 lbs
