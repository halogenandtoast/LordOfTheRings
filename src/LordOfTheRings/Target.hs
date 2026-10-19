module LordOfTheRings.Target (module LordOfTheRings.Target) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Target
  = HeroTarget HeroId
  | AllyTarget AllyId
  | EnemyTarget EnemyId
  | LocationTarget LocationId
  | TreacheryTarget TreacheryId
  | QuestTarget QuestId
  | PlayerTarget PlayerId
  | CardIdTarget CardId
  | ScenarioTarget
  | GameTarget
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

class Targetable a where
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  isTarget = (==) . toTarget

instance Targetable Target where
  toTarget = id
