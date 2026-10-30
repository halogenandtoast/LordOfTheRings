module LordOfTheRings.Enemy.Cards.ScoutingShip (scoutingShip) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ScoutingShip = ScoutingShip EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

scoutingShip :: EnemyCard ScoutingShip
scoutingShip = enemy ScoutingShip Cards.scoutingShip

-- | Boarding 1. Cannot have attachments. Scouting Ship gets -15 engagement cost while you
-- are off-course.
instance HasModifiersFor ScoutingShip where
  getModifiersFor (ScoutingShip a) = do
    off <- isOffCourse
    modified a a $ CannotHaveAttachments : [EngagementCostModifier (-15) | off]
