module LordOfTheRings.Location.Cards.CorsairDromon (corsairDromon) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (corsairEnemy)
import LordOfTheRings.Trait (Trait (Corsair))

newtype CorsairDromon = CorsairDromon LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairDromon :: LocationCard CorsairDromon
corsairDromon = location CorsairDromon Cards.corsairDromon

-- | While Corsair Dromon is the active location, each Corsair enemy gains Archery 1.
instance HasModifiersFor CorsairDromon where
  getModifiersFor (CorsairDromon a) = when (a.placement == ActiveLocation) do
    corsairs <- select (EnemyWithTrait Corsair)
    for_ corsairs \e -> modified a e [GainKeyword (Archery 1)]

-- | Travel: Shuffle the encounter discard pile into the encounter deck and discard cards
-- from the top until a Corsair enemy is discarded. Add the discarded enemy to the staging
-- area. The deck is read for the discards once it is shuffled, a step later.
instance RunMessage CorsairDromon where
  runMessage msg l@(CorsairDromon a) = case msg of
    TravelTo lid | lid == a.id -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 2 msg
      CorsairDromon <$> runMessage msg a
    DoStep 2 (TravelTo lid) | lid == a.id -> do
      discardEncounterCardsUntil a corsairEnemy
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches corsairEnemy) cards) addToStagingFromDiscard
      pure l
    _ -> CorsairDromon <$> runMessage msg a
