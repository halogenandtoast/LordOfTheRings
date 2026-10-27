module LordOfTheRings.Treachery.Cards.InnerShadow (innerShadow) where

import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype InnerShadow = InnerShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

innerShadow :: TreacheryCard InnerShadow
innerShadow = treachery InnerShadow Cards.innerShadow

-- | Surge.
-- When Revealed: Heal 5 damage from Durin's Bane, if able. The first player may remove 1
-- questing hero he controls from the quest to cancel this effect.
instance RunMessage InnerShadow where
  runMessage msg t@(InnerShadow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      bane <- getDurinsBane
      for_ bane \eid -> unlessQuestingHeroLeaves attrs $ healDamage attrs eid 5
      pure t
    _ -> InnerShadow <$> runMessage msg attrs
