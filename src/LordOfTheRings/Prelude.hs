module LordOfTheRings.Prelude (module X, module LordOfTheRings.Prelude) where

import Data.Map.Strict as X (Map)
import Data.Text as X (Text)
import Prelude as X

import Data.Text qualified as T

tshow :: (Show a) => a -> Text
tshow = T.pack . show
