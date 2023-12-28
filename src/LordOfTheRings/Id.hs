module LordOfTheRings.Id (module LordOfTheRings.Id) where

import Control.Monad.Random (Random)
import Data.Aeson
import Data.UUID (UUID)
import LordOfTheRings.Prelude

newtype HeroId = HeroId UUID
  deriving newtype (Random, Show, ToJSON)

newtype PlayerId = PlayerId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq)

newtype CardId = CardId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, Eq)

newtype CostId = CostId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, Eq)
