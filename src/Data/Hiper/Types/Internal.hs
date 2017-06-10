module Data.Hiper.Types.Internal
       (
         Convertible(..)
       , Value(..)
       ) where

import Data.Text

-- | Values stored in the config
data Value = Bool Bool
           | String Text
           | Number Rational
           | List [Value]


class Convertible a where
  convert :: Value -> Maybe a
