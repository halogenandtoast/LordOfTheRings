module LordOfTheRings.Location.Cards.SideStreet (sideStreet) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype SideStreet = SideStreet LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sideStreet :: LocationCard SideStreet
sideStreet = location SideStreet Cards.sideStreet

-- | While Side Street is in the staging area, progress cannot be placed on other locations
-- in the staging area.
instance HasModifiersFor SideStreet where
  getModifiersFor (SideStreet a) = when (a.placement == StagingArea) do
    others <- select (LocationInStagingArea <> not_ (LocationWithId a.id))
    for_ others \l -> modified a l [ProgressLimit 0]

-- | Travel: Shuffle the encounter discard pile into the encounter deck and discard cards
-- until a location is discarded. Add that location to the staging area. The deck is read
-- for the discards once it is shuffled, a step later.
instance RunMessage SideStreet where
  runMessage msg l@(SideStreet a) = case msg of
    TravelTo lid | lid == a.id -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 2 msg
      pure l
    DoStep 2 (TravelTo lid) | lid == a.id -> do
      discardEncounterCardsUntil a (CardWithType LocationType)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches $ CardWithType LocationType) cards) addToStagingFromDiscard
      pure l
    _ -> SideStreet <$> runMessage msg a
