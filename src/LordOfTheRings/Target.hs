module LordOfTheRings.Target (module LordOfTheRings.Target) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Target
  = HeroTarget HeroId
  | AllyTarget AllyId
  | EnemyTarget EnemyId
  | LocationTarget LocationId
  | AttachmentTarget AttachmentId
  | TreacheryTarget TreacheryId
  | ObjectiveTarget ObjectiveId
  | QuestTarget QuestId
  | PlayerTarget PlayerId
  | CardIdTarget CardId
  | ScenarioTarget
  | GameTarget
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

class Targetable a where
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  isTarget = (==) . toTarget

-- | Whether the target is a player card: a hero, ally, attachment or event (an event is
-- its card id), as 'LordOfTheRings.Source.isPlayerCardSource' reads a source.
isPlayerCardTarget :: Target -> Bool
isPlayerCardTarget = \case
  HeroTarget _ -> True
  AllyTarget _ -> True
  AttachmentTarget _ -> True
  CardIdTarget _ -> True
  _ -> False

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

instance Targetable ObjectiveId where
  toTarget = ObjectiveTarget

instance Targetable QuestId where
  toTarget = QuestTarget

instance Targetable PlayerId where
  toTarget = PlayerTarget

instance Targetable AttachmentId where
  toTarget = AttachmentTarget
