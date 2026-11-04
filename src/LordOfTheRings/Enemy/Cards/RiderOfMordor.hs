module LordOfTheRings.Enemy.Cards.RiderOfMordor (riderOfMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Morgul))

newtype RiderOfMordor = RiderOfMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

riderOfMordor :: EnemyCard RiderOfMordor
riderOfMordor = enemy RiderOfMordor Cards.riderOfMordor

-- | Cannot have non-Morgul attachments.
-- While Rider of Mordor is engaged with a player, characters that player controls get -1
-- willpower.
-- While The One Ring is exhausted, Rider of Mordor gets -10 engagement cost.
instance HasModifiersFor RiderOfMordor where
  getModifiersFor (RiderOfMordor a) = do
    exhausted <- theOneRingExhausted
    modifySelf a $ CannotHaveAttachmentsWithoutTrait Morgul : [EngagementCostModifier (-10) | exhausted]
    case a.placement of
      EngagedWith pid -> do
        characters <- charactersControlledBy pid
        for_ characters \c -> modified a c [WillpowerModifier (-1)]
      _ -> pure ()

instance RunMessage RiderOfMordor where
  runMessage msg (RiderOfMordor attrs) = RiderOfMordor <$> runMessage msg attrs
