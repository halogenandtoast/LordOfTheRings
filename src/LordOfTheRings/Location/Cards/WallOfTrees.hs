module LordOfTheRings.Location.Cards.WallOfTrees (wallOfTrees) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype WallOfTrees = WallOfTrees LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

wallOfTrees :: LocationCard WallOfTrees
wallOfTrees = location WallOfTrees Cards.wallOfTrees

instance HasModifiersFor WallOfTrees where
  getModifiersFor (WallOfTrees a) = do
    n <- length <$> getPlayerOrder
    modifySelf a $ [ThreatModifier n, GainVictory n] <> [TakesProgressBeforeCurrentQuest | a.placement == StagingArea]
