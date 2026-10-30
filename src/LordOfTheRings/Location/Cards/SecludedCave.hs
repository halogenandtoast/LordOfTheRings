module LordOfTheRings.Location.Cards.SecludedCave (secludedCave) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype SecludedCave = SecludedCave LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded. Safe. Immune to player card effects.
secludedCave :: LocationCard SecludedCave
secludedCave = location SecludedCave Cards.secludedCave

instance HasModifiersFor SecludedCave where
  getModifiersFor (SecludedCave a) = safeModifiers a

-- | Response: After you travel to Secluded Cave, each player draws 3 cards.
instance HasAbilities SecludedCave where
  getAbilities (SecludedCave a) = mkAbility a 1 (response $ TraveledWindow After a.id) : safeAbilities a

instance RunMessage SecludedCave where
  runMessage msg l@(SecludedCave a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> drawCards a pid 3
      pure l
    _ -> do
      runSafe msg a
      SecludedCave <$> runMessage msg a
