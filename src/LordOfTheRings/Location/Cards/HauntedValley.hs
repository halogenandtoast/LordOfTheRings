module LordOfTheRings.Location.Cards.HauntedValley (hauntedValley) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Helpers.Window (exhaustableHeroes)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Sphere (Sphere (Lore, Leadership, Spirit, Tactics))

newtype HauntedValley = HauntedValley LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

hauntedValley :: LocationCard HauntedValley
hauntedValley = location HauntedValley Cards.hauntedValley

instance HasModifiersFor HauntedValley where
  getModifiersFor (HauntedValley a) = when (a.placement == StagingArea) do
    heroes <- select ReadyHero
    for_ heroes \hid -> modified a hid $ map LoseResourceIcon [Lore, Leadership, Spirit, Tactics]

instance HasAbilities HauntedValley where
  getAbilities (HauntedValley a) =
    [anyPlayer $ restricted a 1 (DuringPhase PlanningPhase <> exists (ExhaustableHero)) $ ActionAbility Free]

instance RunMessage HauntedValley where
  runMessage msg l@(HauntedValley a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- exhaustableHeroes AnyHero
      chooseOneM pid do
        questionLabeled "erech.hauntedHero"
        causedBy a
        targets heroes exhaust
      pure l
    _ -> HauntedValley <$> runMessage msg a
