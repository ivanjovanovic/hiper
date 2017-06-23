module Data.Hiper.Types.Internal
       (
         Convertible(..)
       , Value(..)
       ) where

import Data.Text
import Data.Scientific

-- | Values stored in the config
data Value = Bool Bool
           | String Text
           | Number Scientific
           | List [Value]
           | Null
             deriving (Show)


class Convertible a where
  fromValue :: Value -> Maybe a
  toValue :: a -> Maybe Value

  fromValueList :: Value -> Maybe [a]
  fromValueList (List xs) = mapM fromValue xs
  fromValueList _ = Nothing

  toValueList :: [a] -> Maybe Value
  toValueList xs = case mapM toValue xs of
    Just vs -> Just (List vs)
    _ -> Nothing
