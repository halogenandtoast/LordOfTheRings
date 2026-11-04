module LordOfTheRings.Location.Cards.BurningWatchtower (burningWatchtower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (sideQuestX)

newtype BurningWatchtower = BurningWatchtower LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

burningWatchtower :: LocationCard BurningWatchtower
burningWatchtower = location BurningWatchtower Cards.burningWatchtower

-- | X is 1 more than the number of side quests in the victory display. Travel: Each player
-- assigns X damage among characters he controls.
instance RunMessage BurningWatchtower where
  runMessage msg l@(BurningWatchtower a) = case msg of
    TravelTo lid | lid == a.id -> do
      x <- sideQuestX
      eachPlayer \p -> dealDamageDivided a p x
      pure l
    _ -> BurningWatchtower <$> runMessage msg a
