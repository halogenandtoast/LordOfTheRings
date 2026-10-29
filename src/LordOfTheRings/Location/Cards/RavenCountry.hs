module LordOfTheRings.Location.Cards.RavenCountry (ravenCountry) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (activeLocationTimeX, assignDamage)

newtype RavenCountry = RavenCountry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenCountry :: LocationCard RavenCountry
ravenCountry = location RavenCountry Cards.ravenCountry

-- | Forced: After the last time counter is removed from this location, each player raises
-- his threat by the number of cards in his hand.
instance HasAbilities RavenCountry where
  getAbilities (RavenCountry a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Shadow: Defending player assigns X damage among characters he controls. X is the Time
-- X value on the active location.
instance RunMessage RavenCountry where
  runMessage msg l@(RavenCountry a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        hand <- length . (.hand) <$> getPlayer pid
        encounterRaisesThreat pid hand
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      assignDamage a pid =<< activeLocationTimeX
      pure l
    _ -> RavenCountry <$> runMessage msg a
