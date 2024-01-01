module LordOfTheRings.Prelude (module X, module LordOfTheRings.Prelude) where

import Data.Aeson as X (FromJSON, ToJSON (toJSON))
import Data.Map.Strict as X (Map)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import GHC.Records as X (HasField (..))
import Prelude as X

import Data.Text qualified as T

tshow :: (Show a) => a -> Text
tshow = T.pack . show
