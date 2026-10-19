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

instance Targetable HeroId where
  toTarget = HeroTarget

instance Targetable AllyId where
  toTarget = AllyTarget

instance Targetable EnemyId where
  toTarget = EnemyTarget

instance Targetable LocationId where
  toTarget = LocationTarget

instance Targetable TreacheryId where
  toTarget = TreacheryTarget

instance Targetable QuestId where
  toTarget = QuestTarget

instance Targetable PlayerId where
  toTarget = PlayerTarget
