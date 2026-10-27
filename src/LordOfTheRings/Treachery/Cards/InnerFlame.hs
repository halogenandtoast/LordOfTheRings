module LordOfTheRings.Treachery.Cards.InnerFlame (innerFlame) where

import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype InnerFlame = InnerFlame TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

innerFlame :: TreacheryCard InnerFlame
innerFlame = treachery InnerFlame Cards.innerFlame

-- | When Revealed: Durin's Bane gets +3 attack until the end of the round. The first player
-- may remove 1 questing hero he controls from the quest to cancel this effect.
-- Shadow: If attacking enemy is Durin's Bane, it gets +3 attack.
instance RunMessage InnerFlame where
  runMessage msg t@(InnerFlame attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      bane <- getDurinsBane
      for_ bane \eid -> unlessQuestingHeroLeaves attrs $ roundModifier attrs eid (AttackModifier 3)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      whenM (isDurinsBane eid) $ enemyGetsAttack attrs eid 3
      pure t
    _ -> InnerFlame <$> runMessage msg attrs
