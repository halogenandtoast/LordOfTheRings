module LordOfTheRings.Enemy.Cards.MorgulWraith (morgulWraith) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Morgul))

newtype MorgulWraith = MorgulWraith EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulWraith :: EnemyCard MorgulWraith
morgulWraith = enemy MorgulWraith Cards.morgulWraith

-- | Cannot have non-Morgul attachments.
-- While The One Ring is exhausted, Morgul Wraith gets -30 engagement cost and gains
-- "immune to player card effects."
instance HasModifiersFor MorgulWraith where
  getModifiersFor (MorgulWraith a) = do
    exhausted <- theOneRingExhausted
    modifySelf a
      $ CannotHaveAttachmentsWithoutTrait Morgul
      : (if exhausted then [EngagementCostModifier (-30), ImmuneToPlayerCardEffects] else [])
