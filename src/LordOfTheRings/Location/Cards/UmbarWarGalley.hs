module LordOfTheRings.Location.Cards.UmbarWarGalley (umbarWarGalley) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (corsairEnemy)
import LordOfTheRings.Trait (Trait (Corsair))

newtype UmbarWarGalley = UmbarWarGalley LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarWarGalley :: LocationCard UmbarWarGalley
umbarWarGalley = location UmbarWarGalley Cards.umbarWarGalley

-- | While Umbar War Galley is the active location, each Corsair enemy gets -10 engagement
-- cost.
instance HasModifiersFor UmbarWarGalley where
  getModifiersFor (UmbarWarGalley a) = when (a.placement == ActiveLocation) do
    corsairs <- select (EnemyWithTrait Corsair)
    for_ corsairs \e -> modified a e [EngagementCostModifier (-10)]

-- | Travel: Shuffle the encounter discard pile into the encounter deck and discard cards
-- from the encounter deck until a Corsair enemy is discarded. Add the discarded enemy to the
-- staging area. The deck is read for the discards once it is shuffled, a step later.
instance RunMessage UmbarWarGalley where
  runMessage msg l@(UmbarWarGalley a) = case msg of
    TravelTo lid | lid == a.id -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 2 msg
      UmbarWarGalley <$> runMessage msg a
    DoStep 2 (TravelTo lid) | lid == a.id -> do
      discardEncounterCardsUntil a corsairEnemy
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches corsairEnemy) cards) addToStagingFromDiscard
      pure l
    _ -> UmbarWarGalley <$> runMessage msg a
