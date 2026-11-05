module LordOfTheRings.Location.Cards.FortressOfIsengard (fortressOfIsengard) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (wizardry)
import LordOfTheRings.Trait (Trait (Orc))

newtype FortressOfIsengard = FortressOfIsengard LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

fortressOfIsengard :: LocationCard FortressOfIsengard
fortressOfIsengard = location FortressOfIsengard Cards.fortressOfIsengard

instance HasModifiersFor FortressOfIsengard where
  getModifiersFor (FortressOfIsengard a) = when (a.placement == StagingArea) do
    enemies <- select (EnemyWithTrait Orc)
    for_ enemies \eid -> modified a eid [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]

instance HasAbilities FortressOfIsengard where
  getAbilities (FortressOfIsengard a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage FortressOfIsengard where
  runMessage msg l@(FortressOfIsengard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      wizardry a (toCardCode a) =<< getRemainingPlayers
      pure l
    _ -> FortressOfIsengard <$> runMessage msg a
