module LordOfTheRings.Location.Cards.TraitorsDen (traitorsDen) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (revealRemovedPlot)

newtype TraitorsDen = TraitorsDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

traitorsDen :: LocationCard TraitorsDen
traitorsDen = location TraitorsDen Cards.traitorsDen

-- | Underworld 1 (see the scenario).
-- Forced: At the end of the round, place 1 resource token on Traitors' Den. Then, if it
-- has 4 or more resource tokens on it, remove all tokens from it and reveal at random one
-- of the Plot cards removed from the game during setup. Add that card to the staging area.
instance HasAbilities TraitorsDen where
  getAbilities (TraitorsDen a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage TraitorsDen where
  runMessage msg l@(TraitorsDen a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      let n = countTokens Resource a.locationTokens
      when (n >= 4) do
        removeTokens a a Resource n
        revealRemovedPlot
      pure l
    _ -> TraitorsDen <$> runMessage msg a
