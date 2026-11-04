module LordOfTheRings.Location.Cards.TheOldRoad (theOldRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheOldRoad = TheOldRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldRoad :: LocationCard TheOldRoad
theOldRoad = location TheOldRoad Cards.theOldRoad

-- | Action: Place the top card of the burden deck on the encounter deck to place 4
-- progress on The Old Road. (Use this action only if you are playing campaign mode.)
-- Any player may use it, as an encounter card's action.
instance HasAbilities TheOldRoad where
  getAbilities (TheOldRoad a) = [anyPlayer $ restricted a 1 (InCampaignMode <> BurdenDeckNotEmpty) $ ActionAbility Free]

instance RunMessage TheOldRoad where
  runMessage msg l@(TheOldRoad a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeTopOfBurdenDeckOnEncounterDeck
      placeProgress a a 4
      pure l
    _ -> TheOldRoad <$> runMessage msg a
