module LordOfTheRings.Location.Cards.ContestedDepths (contestedDepths) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheKingsQuest.Helpers (locationsInVictoryDisplay)

newtype ContestedDepths = ContestedDepths LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

contestedDepths :: LocationCard ContestedDepths
contestedDepths = location ContestedDepths Cards.contestedDepths

-- | Contested Depths gets +1 threat for each location in the victory display.
instance HasModifiersFor ContestedDepths where
  getModifiersFor (ContestedDepths a) = do
    n <- length <$> locationsInVictoryDisplay
    when (n > 0) $ modifySelf a [ThreatModifier n]

-- | Forced: After the players travel to Contested Depths, either raise each player's
-- threat by 1 for each location in the victory display, or discard Contested Depths and
-- replace it with the top card of the Caves deck.
instance HasAbilities ContestedDepths where
  getAbilities (ContestedDepths a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | The first player chooses. With the Caves deck empty Contested Depths cannot be
-- replaced, so only the threat is offered: an option chosen must be performed in full, if
-- able (FAQ 1.44).
instance RunMessage ContestedDepths where
  runMessage msg l@(ContestedDepths a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      n <- length <$> locationsInVictoryDisplay
      caves <- getCavesDeck
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "contestedDepths.choose"
        causedBy a
        labeledWith "contestedDepths.threat" [intVar "n" n] $ eachPlayer (`raiseThreat` n)
        unless (null caves) $ labeled "contestedDepths.replace" $ replaceWithTopOfCavesDeck a.id
      pure l
    _ -> ContestedDepths <$> runMessage msg a
