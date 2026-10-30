module LordOfTheRings.Location.Cards.BrokenBattlements (brokenBattlements) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype BrokenBattlements = BrokenBattlements LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brokenBattlements :: LocationCard BrokenBattlements
brokenBattlements = location BrokenBattlements Cards.brokenBattlements

-- | Time 3. Forced: After the last time counter is removed from Broken Battlements, each
-- player discards the top 5 cards of his deck and places 1 time counter here.
instance HasAbilities BrokenBattlements where
  getAbilities (BrokenBattlements a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Travel: Each player discards the top card of his deck.
instance RunMessage BrokenBattlements where
  runMessage msg l@(BrokenBattlements a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> discardTopOfDeck p 5
      placeTimeCounters a a 1
      pure l
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> discardTopOfDeck p 1
      pure l
    _ -> BrokenBattlements <$> runMessage msg a
