module LordOfTheRings.Location.Cards.BareHilltop (bareHilltop) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers

newtype BareHilltop = BareHilltop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An objective-location: Guarded (enemy). Searching for a Way Out makes it immune to
-- player card effects.
bareHilltop :: LocationCard BareHilltop
bareHilltop = location BareHilltop Cards.bareHilltop

instance HasAbilities BareHilltop where
  getAbilities (BareHilltop a) = objectiveLocationAbilities a

{- | Travel: Shuffle the encounter discard pile into the encounter deck. Each player discards the top card of the
encounter deck. Add each location discarded this way to the staging area.
Forced: After the players travel here, advance to the next stage A, if able.
-}
instance RunMessage BareHilltop where
  runMessage msg l@(BareHilltop a) = case msg of
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget _) cards -> do
      for_ (filter (cardMatches (CardWithType LocationType)) cards) addToStagingFromDiscard
      pure l
    _ -> do
      runObjectiveLocation SideA msg a
      BareHilltop <$> runMessage msg a
