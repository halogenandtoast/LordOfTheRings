module LordOfTheRings.Id where

import Control.Monad.Random (Random)
import Data.Aeson
import Data.UUID (UUID)

newtype HeroId = HeroId UUID
  deriving newtype (Random, Show, ToJSON)

newtype PlayerId = PlayerId UUID
  deriving newtype (Random, Show, ToJSON, ToJSONKey)

newtype CardId = CardId UUID
  deriving newtype (Random, Show, ToJSON)
