module LordOfTheRings.Location.Cards.BranchingPaths (branchingPaths) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Dark))

newtype BranchingPaths = BranchingPaths LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

branchingPaths :: LocationCard BranchingPaths
branchingPaths = location BranchingPaths Cards.branchingPaths

-- | While Branching Paths is in the staging area, each Dark location gets +1 [Threat].
instance HasModifiersFor BranchingPaths where
  getModifiersFor (BranchingPaths a) = when (a.placement == StagingArea) do
    dark <- select (LocationWithTrait Dark)
    for_ dark \l -> modified a l [ThreatModifier 1]

-- | Forced: After Branching Paths leaves play as an explored location, look at the top 3
-- cards of the encounter deck. Players must choose 1 of those to reveal and add to the
-- staging area, moving the other 2 to the bottom of the deck.
instance HasAbilities BranchingPaths where
  getAbilities (BranchingPaths a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage BranchingPaths where
  runMessage msg l@(BranchingPaths a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      revealOneOfTopEncounterCardsRestToBottom lead a 3
      pure l
    _ -> BranchingPaths <$> runMessage msg a
