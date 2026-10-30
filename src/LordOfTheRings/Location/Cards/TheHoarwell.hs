module LordOfTheRings.Location.Cards.TheHoarwell (theHoarwell) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype TheHoarwell = TheHoarwell LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded. Safe. Immune to player card effects.
theHoarwell :: LocationCard TheHoarwell
theHoarwell = location TheHoarwell Cards.theHoarwell

instance HasModifiersFor TheHoarwell where
  getModifiersFor (TheHoarwell a) = safeModifiers a

-- | Response: After you travel to The Hoarwell, heal 1 damage from each character in play.
instance HasAbilities TheHoarwell where
  getAbilities (TheHoarwell a) = mkAbility a 1 (response $ TraveledWindow After a.id) : safeAbilities a

instance RunMessage TheHoarwell where
  runMessage msg l@(TheHoarwell a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      for_ damaged \c -> healDamage a c 1
      pure l
    _ -> do
      runSafe msg a
      TheHoarwell <$> runMessage msg a
