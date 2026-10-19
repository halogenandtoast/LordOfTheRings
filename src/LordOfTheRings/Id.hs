module LordOfTheRings.Id (module LordOfTheRings.Id) where

import LordOfTheRings.Prelude

newtype LocationId = LocationId {unLocationId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EnemyId = EnemyId {unEnemyId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype TreacheryId = TreacheryId {unTreacheryId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype QuestId = QuestId {unQuestId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype HeroId = HeroId {unHeroId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AllyId = AllyId {unAllyId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype PlayerId = PlayerId {unPlayerId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype CardId = CardId {unCardId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype CostId = CostId {unCostId :: UUID}
  deriving newtype (Show, Eq, Ord, Random, ToJSON, FromJSON)

newtype ScenarioId = ScenarioId {unScenarioId :: Text}
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
