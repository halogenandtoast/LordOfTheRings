module LordOfTheRings.Location.Cards.OldSouthRoad (oldSouthRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype OldSouthRoad = OldSouthRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

oldSouthRoad :: LocationCard OldSouthRoad
oldSouthRoad = location OldSouthRoad Cards.oldSouthRoad

-- | Forced: At the end of the refresh phase, remove 1 time counter from the current quest.
instance HasAbilities OldSouthRoad where
  getAbilities (OldSouthRoad a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]

instance RunMessage OldSouthRoad where
  runMessage msg l@(OldSouthRoad a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeTimeCountersFromCurrentQuest a 1
      pure l
    _ -> OldSouthRoad <$> runMessage msg a
