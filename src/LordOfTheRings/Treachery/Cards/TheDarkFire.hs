module LordOfTheRings.Treachery.Cards.TheDarkFire (theDarkFire) where

import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers (getDurinsBane)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheDarkFire = TheDarkFire TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Shadow and Flame's Balrog deck, in nightmare mode.
theDarkFire :: TreacheryCard TheDarkFire
theDarkFire = treachery TheDarkFire Cards.theDarkFire

-- | Shadow: For each point of damage dealt by this attack, heal 1 damage from Durin's Bane.
instance RunMessage TheDarkFire where
  runMessage msg t@(TheDarkFire attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      bane <- getDurinsBane
      for_ bane \b -> attackModifier attrs eid (AttackDamageHeals $ toTarget b)
      pure t
    _ -> TheDarkFire <$> runMessage msg attrs
