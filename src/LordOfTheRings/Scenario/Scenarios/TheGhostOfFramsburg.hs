module LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg (theGhostOfFramsburg) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheGhostOfFramsburg = TheGhostOfFramsburg ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGhostOfFramsburg :: Scenario
theGhostOfFramsburg = Scenario $ scenario TheGhostOfFramsburg "the-ghost-of-framsburg" "The Ghost of Framsburg"

{- | The encounter deck is built from The Ghost of Framsburg, Wild Creatures and Afraid of
the Dark (MEC69 rules). The Cursed Tomb is set aside as it is gathered, as stage 1's setup
reads. The players win as they defeat stage 3, the last.
-}
instance RunMessage TheGhostOfFramsburg where
  runMessage msg (TheGhostOfFramsburg attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheGhostOfFramsburg, EncounterSet.WildCreatures, EncounterSet.AfraidOfTheDark]
      let (aside, encounterDeck) = partition (cardMatches $ cardIs Locations.theCursedTomb) cards
      firstStage <- genEncounterCard Quests.theHauntedKeep
      laterStages <- traverse genEncounterCard [Quests.searchingTheRuins, Quests.theCursedShade]
      putQuestIntoPlay firstStage
      pure
        $ TheGhostOfFramsburg
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    _ -> TheGhostOfFramsburg <$> runMessage msg attrs
