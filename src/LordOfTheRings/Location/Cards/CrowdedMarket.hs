module LordOfTheRings.Location.Cards.CrowdedMarket (crowdedMarket) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (getCityGuard)

newtype CrowdedMarket = CrowdedMarket LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

crowdedMarket :: LocationCard CrowdedMarket
crowdedMarket = location CrowdedMarket Cards.crowdedMarket

-- | While Crowded Market is the active location, City Guard gets +20 engagement cost.
instance HasModifiersFor CrowdedMarket where
  getModifiersFor (CrowdedMarket a) = when (a.placement == ActiveLocation) do
    getCityGuard >>= traverse_ \g -> modified a (EnemyTarget g) [EngagementCostModifier 20]

-- | Travel: Shuffle the encounter discard pile into the encounter deck and discard cards
-- from the top until an enemy is discarded. Add that enemy to the staging area. The deck is
-- read for the discards once it is shuffled, a step later.
instance RunMessage CrowdedMarket where
  runMessage msg l@(CrowdedMarket a) = case msg of
    TravelTo lid | lid == a.id -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 2 msg
      CrowdedMarket <$> runMessage msg a
    DoStep 2 (TravelTo lid) | lid == a.id -> do
      discardEncounterCardsUntil a enemyCard
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches enemyCard) cards) addToStagingFromDiscard
      pure l
    _ -> CrowdedMarket <$> runMessage msg a
   where
    enemyCard = CardWithType EnemyType
