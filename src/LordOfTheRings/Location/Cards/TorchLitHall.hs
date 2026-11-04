module LordOfTheRings.Location.Cards.TorchLitHall (torchLitHall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTowerOfCirithUngol.Helpers (orcsFight, resolveOrcsFight)

newtype TorchLitHall = TorchLitHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

torchLitHall :: LocationCard TorchLitHall
torchLitHall = location TorchLitHall Cards.torchLitHall

-- | Forced: After the players travel to Torch-lit Hall, discard cards from the top of the
-- encounter deck until an enemy is discarded. If there is a copy of that enemy in the
-- staging area, discard that copy as well. If there is not, add the discarded enemy to the
-- staging area.
instance HasAbilities TorchLitHall where
  getAbilities (TorchLitHall a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage TorchLitHall where
  runMessage msg l@(TorchLitHall a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      orcsFight a a
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      resolveOrcsFight cards
      pure l
    _ -> TorchLitHall <$> runMessage msg a
