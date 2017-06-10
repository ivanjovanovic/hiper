module Data.Hiper.Instances () where

import Data.Text.Encoding (encodeUtf8)
import Data.Hiper.Types.Internal
import Data.Ratio (Ratio, denominator, numerator)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

instance Convertible Value where
  convert = Just

instance Convertible Bool where
  convert (Bool v) = Just v
  convert _ = Nothing

convertNumberToNum :: (Num a) => Value -> Maybe a
convertNumberToNum (Number r)
  | denominator r == 1 = Just $ fromInteger $ numerator r
convertNumberToNum _ = Nothing

instance Convertible Int where
  convert = convertNumberToNum

instance Convertible Integer where
  convert = convertNumberToNum

instance Convertible Int8 where
    convert = convertNumberToNum

instance Convertible Int16 where
    convert = convertNumberToNum

instance Convertible Int32 where
    convert = convertNumberToNum

instance Convertible Int64 where
    convert = convertNumberToNum

instance Convertible Word where
    convert = convertNumberToNum

instance Convertible Word8 where
    convert = convertNumberToNum

instance Convertible Word16 where
    convert = convertNumberToNum

instance Convertible Word32 where
    convert = convertNumberToNum

instance Convertible Word64 where
    convert = convertNumberToNum

convertNumberToFractional :: (Fractional a) => Value -> Maybe a
convertNumberToFractional (Number r) = Just $ fromRational r
convertNumberToFractional _ = Nothing

instance Convertible Double where
    convert = convertNumberToFractional

instance Convertible Float where
    convert = convertNumberToFractional

-- instance Convertible CDouble where
--     convert = convertNumberToFractional

-- instance Convertible CFloat where
--     convert = convertNumberToFractional

-- instance Integral a => Convertible (Ratio a) where
--     convert = convertNumberToFractional

-- instance RealFloat a => Convertible (Complex a) where
--     convert = convertNumberToFractional

-- instance HasResolution a => Convertible (Fixed a) where
--     convert = convertNumberToFractional

instance Convertible T.Text where
  convert (String t) = Just t
  convert _ = Nothing

instance Convertible Char where
  convert (String t) | T.length t == 1 = Just $ T.head t
  convert _ = Nothing

instance Convertible L.Text where
  convert = fmap L.fromStrict . convert

instance Convertible B.ByteString where
  convert = fmap encodeUtf8 . convert

instance Convertible LB.ByteString where
  convert = fmap (LB.fromChunks . (:[])) . convert
