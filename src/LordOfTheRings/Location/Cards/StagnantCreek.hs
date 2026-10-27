module LordOfTheRings.Location.Cards.StagnantCreek (stagnantCreek) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (isTentacleEnemy)

newtype StagnantCreek = StagnantCreek LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stagnantCreek :: LocationCard StagnantCreek
stagnantCreek = location StagnantCreek Cards.stagnantCreek

-- | When Revealed: Discard the top card of the encounter deck. If the discarded card is a
-- Tentacle enemy, add that card to the staging area and raise each player's threat by 5.
instance RunMessage StagnantCreek where
  runMessage msg l@(StagnantCreek a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      discardTopOfEncounterDeck a 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter isTentacleEnemy cards) \card -> do
        addToStagingFromDiscard card
        eachPlayer \p -> encounterRaisesThreat p 5
      pure l
    _ -> StagnantCreek <$> runMessage msg a
