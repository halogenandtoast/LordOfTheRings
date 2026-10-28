module LordOfTheRings.Quest.Cards.TheGrandDesign (theGrandDesign) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (investigation, runInvestigation)
import LordOfTheRings.Trait (Trait (Plot))

newtype TheGrandDesign = TheGrandDesign QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGrandDesign :: QuestCard TheGrandDesign
theGrandDesign = quest TheGrandDesign Cards.theGrandDesign

-- | Forced: After the active location leaves play as an explored location place 1
-- resource token on this quest. If there are 4 or more resource tokens on The Grand
-- Design, advance to the next stage.
instance HasAbilities TheGrandDesign where
  getAbilities (TheGrandDesign a) = [investigation a]

-- | When Revealed: Reveal the set aside Plot card and add it to the staging area.
-- When Revealed: Make Roots of Mindolluin the active location, returning any other active
-- location to the staging area.
instance RunMessage TheGrandDesign where
  runMessage msg q@(TheGrandDesign attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheGrandDesign <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (CardWithTrait Plot)
      traverse_ (`placeLocation` StagingArea) =<< select IsActiveLocation
      setAside' <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ putIntoPlayAsActiveLocation $ filter ((== toCardCode Locations.rootsOfMindolluin) . toCardCode) setAside'
      pure q
    _ -> TheGrandDesign <$> runInvestigation msg attrs
