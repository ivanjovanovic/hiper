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
  convert :: Value -> Maybe a
