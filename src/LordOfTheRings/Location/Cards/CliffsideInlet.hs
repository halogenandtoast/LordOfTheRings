module LordOfTheRings.Location.Cards.CliffsideInlet (cliffsideInlet) where

import LordOfTheRings.Card (CardType (EnemyType), EncounterCard)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (addToStormcallerArea)
import LordOfTheRings.Trait (Trait (Ship))

newtype CliffsideInlet = CliffsideInlet LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cliffsideInlet :: LocationCard CliffsideInlet
cliffsideInlet = location CliffsideInlet Cards.cliffsideInlet

-- | While Cliffside Inlet is in the staging area, the main quest gets +2 quest points.
instance HasModifiersFor CliffsideInlet where
  getModifiersFor (CliffsideInlet a) = when (a.placement == StagingArea) do
    quests <- getMainQuests
    for_ quests \q -> modified a q [QuestPointModifier 2]

shipEnemyCard :: CardMatcher
shipEnemyCard = CardWithType EnemyType <> CardWithTrait Ship

-- | Travel: Discard cards from the top of the encounter deck until a Ship enemy is
-- discarded. Put that enemy into play in the Stormcaller's area.
instance RunMessage CliffsideInlet where
  runMessage msg (CliffsideInlet a) = case msg of
    TravelTo lid | lid == a.id -> do
      discardEncounterCardsUntil a shipEnemyCard
      CliffsideInlet <$> runMessage msg a
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches shipEnemyCard) cards) \(card :: EncounterCard) -> do
        removeFromEncounterDiscard card
        eid <- getRandom
        push $ CreateEnemy eid card
        addToStormcallerArea (EnemyTarget eid)
      pure $ CliffsideInlet a
    _ -> CliffsideInlet <$> runMessage msg a
