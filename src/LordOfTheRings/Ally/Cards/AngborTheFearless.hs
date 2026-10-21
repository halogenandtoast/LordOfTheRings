module LordOfTheRings.Ally.Cards.AngborTheFearless (angborTheFearless) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype AngborTheFearless = AngborTheFearless AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

angborTheFearless :: AllyCard AngborTheFearless
angborTheFearless = ally AngborTheFearless Cards.angborTheFearless

-- | While your threat is 40 or higher, Angbor the Fearless gets +2 attack and does not
-- exhaust to quest.
instance HasModifiersFor AngborTheFearless where
  getModifiersFor (AngborTheFearless a) = for_ (placementController a.placement) \pid -> do
    threat <- (.threat) <$> getPlayer pid
    when (threat >= 40) $ modifySelf a [AttackModifier 2, DoesNotExhaustToCommit]
