module LordOfTheRings.Location.Cards.ForestClearingLostInMirkwood (forestClearingLostInMirkwood) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers

newtype ForestClearingLostInMirkwood = ForestClearingLostInMirkwood LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An objective-location: Guarded (enemy). Searching for a Way Out makes it immune to
-- player card effects.
forestClearingLostInMirkwood :: LocationCard ForestClearingLostInMirkwood
forestClearingLostInMirkwood = location ForestClearingLostInMirkwood Cards.forestClearingLostInMirkwood

instance HasAbilities ForestClearingLostInMirkwood where
  getAbilities (ForestClearingLostInMirkwood a) = objectiveLocationAbilities a

{- | Travel: Shuffle the encounter discard pile into the encounter deck. Each player discards the top card of the
encounter deck and raises his threat by X, where X is that card's threat.
Forced: After the players travel here, advance to the next stage C, if able.
-}
instance RunMessage ForestClearingLostInMirkwood where
  runMessage msg l@(ForestClearingLostInMirkwood a) = case msg of
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      for_ cards \c -> raiseThreat pid (fromMaybe 0 $ cdThreat $ toCardDef c)
      pure l
    _ -> do
      runObjectiveLocation SideC msg a
      ForestClearingLostInMirkwood <$> runMessage msg a
