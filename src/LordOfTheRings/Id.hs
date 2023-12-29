module LordOfTheRings.Id (module LordOfTheRings.Id) where

import Control.Monad.Random (Random)
import Data.Aeson
import Data.UUID (UUID)
import LordOfTheRings.Prelude

newtype QuestId = QuestId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype HeroId = HeroId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype AllyId = AllyId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype PlayerId = PlayerId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, ToJSONKey, Eq, Ord)

newtype CardId = CardId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, Eq)

newtype CostId = CostId UUID
  deriving newtype (Random, Show, ToJSON, FromJSON, Eq)
