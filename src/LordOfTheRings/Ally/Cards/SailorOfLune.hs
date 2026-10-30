module LordOfTheRings.Ally.Cards.SailorOfLune (sailorOfLune) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Placement (placementController)

newtype SailorOfLune = SailorOfLune AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

sailorOfLune :: AllyCard SailorOfLune
sailorOfLune = ally SailorOfLune Cards.sailorOfLune

-- | While the top card of your discard pile is an event, Sailor of Lune gets +1 willpower
-- and gains: "Cannot be damaged while committed to the quest."
instance HasModifiersFor SailorOfLune where
  getModifiersFor (SailorOfLune a) = for_ (placementController a.placement) \pid ->
    whenM (topOfDiscardIs EventType pid)
      $ modifySelf a
      $ WillpowerModifier 1
      : [CannotTakeDamage | allyCommitted a]
