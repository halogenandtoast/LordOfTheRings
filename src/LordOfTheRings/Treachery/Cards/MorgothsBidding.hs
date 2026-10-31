module LordOfTheRings.Treachery.Cards.MorgothsBidding (morgothsBidding) where

import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers (getDurinsBane)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MorgothsBidding = MorgothsBidding TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Shadow and Flame's Balrog deck, in nightmare mode.
morgothsBidding :: TreacheryCard MorgothsBidding
morgothsBidding = treachery MorgothsBidding Cards.morgothsBidding

-- | Shadow: If this attack destroys a character, Durin's Bane makes an additional attack
-- against the defending player after this one.
instance RunMessage MorgothsBidding where
  runMessage msg t@(MorgothsBidding attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      bane <- getDurinsBane
      for_ bane (immediateAttack pid)
      pure t
    _ -> MorgothsBidding <$> runMessage msg attrs
