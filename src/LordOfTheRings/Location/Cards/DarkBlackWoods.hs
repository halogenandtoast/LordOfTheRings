module LordOfTheRings.Location.Cards.DarkBlackWoods (darkBlackWoods) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Forest))

newtype DarkBlackWoods = DarkBlackWoods LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkBlackWoods :: LocationCard DarkBlackWoods
darkBlackWoods = location DarkBlackWoods Cards.darkBlackWoods

-- | While Dark Black Woods is in the staging area, no more than 1 progress can be placed on
-- each Forest location in the staging area each round.
instance HasModifiersFor DarkBlackWoods where
  getModifiersFor (DarkBlackWoods a) = when (a.placement == StagingArea) do
    forests <- select (LocationInStagingArea <> LocationWithTrait Forest)
    for_ forests \l -> modified a l [ProgressPerRoundLimit 1]

-- | Travel: Each player discards the top card of the encounter deck. Add each enemy
-- discarded this way to the staging area.
instance RunMessage DarkBlackWoods where
  runMessage msg l@(DarkBlackWoods a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> discardTopOfEncounterDeckFor a (PlayerTarget p) 1
      DarkBlackWoods <$> runMessage msg a
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches (CardWithType EnemyType)) cards) addToStagingFromDiscard
      pure l
    _ -> DarkBlackWoods <$> runMessage msg a
