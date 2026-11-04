module LordOfTheRings.Location.Cards.AbandonedVillage (abandonedVillage) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers

newtype AbandonedVillage = AbandonedVillage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An objective-location: Guarded (location). Searching for a Way Out makes it immune to
-- player card effects.
abandonedVillage :: LocationCard AbandonedVillage
abandonedVillage = location AbandonedVillage Cards.abandonedVillage

instance HasAbilities AbandonedVillage where
  getAbilities (AbandonedVillage a) = objectiveLocationAbilities a

{- | Travel: Shuffle the encounter discard pile into the encounter deck. Each player discards the top card of the
encounter deck. Add each enemy discarded this way to the staging area.
Forced: After the players travel here, advance to the next stage C, if able.
-}
instance RunMessage AbandonedVillage where
  runMessage msg l@(AbandonedVillage a) = case msg of
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget _) cards -> do
      for_ (filter (cardMatches (CardWithType EnemyType)) cards) addToStagingFromDiscard
      pure l
    _ -> do
      runObjectiveLocation SideC msg a
      AbandonedVillage <$> runMessage msg a
