module LordOfTheRings.Ally.Cards.EregionSurvivor (eregionSurvivor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype EregionSurvivor = EregionSurvivor AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eregionSurvivor :: AllyCard EregionSurvivor
eregionSurvivor = ally EregionSurvivor Cards.eregionSurvivor

-- | While you have no cards in your hand, Eregion Survivor gets +1 willpower, +1 attack,
-- and +1 defense.
instance HasModifiersFor EregionSurvivor where
  getModifiersFor (EregionSurvivor a) = for_ (placementController a.placement) \pid -> do
    noHand <- null . (.hand) <$> getPlayer pid
    when noHand $ modifySelf a [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
