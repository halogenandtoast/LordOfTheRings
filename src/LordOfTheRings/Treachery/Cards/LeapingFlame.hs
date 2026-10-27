module LordOfTheRings.Treachery.Cards.LeapingFlame (leapingFlame) where

import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LeapingFlame = LeapingFlame TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

leapingFlame :: TreacheryCard LeapingFlame
leapingFlame = treachery LeapingFlame Cards.leapingFlame

-- | When Revealed: Durin's Bane makes an immediate attack against the first player (deal
-- and resolve a shadow card).
-- Shadow: If attacking enemy is Durin's Bane, it gets +3 attack.
instance RunMessage LeapingFlame where
  runMessage msg t@(LeapingFlame attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      traverse_ (immediateAttack lead) =<< getDurinsBane
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      whenM (isDurinsBane eid) $ enemyGetsAttack attrs eid 3
      pure t
    _ -> LeapingFlame <$> runMessage msg attrs
