module LordOfTheRings.Scenario.Scenarios.TheBlackSerpent (theBlackSerpent) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardWithTrait, CardWithType), cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Trait (Trait (Harad))

newtype TheBlackSerpent = TheBlackSerpent ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBlackSerpent :: Scenario
theBlackSerpent = Scenario $ scenario TheBlackSerpent "the-black-serpent" "The Black Serpent"

{- | The encounter deck is built from The Black Serpent and Harad Soldiers (MEC59 rules).
The Harad objectives and The Black Serpent are set aside as it is gathered, for stage 1's
setup to add The Black Serpent to the staging area and reveal an objective. The players
win as they defeat stage 3, the last.
-}
instance RunMessage TheBlackSerpent where
  runMessage msg (TheBlackSerpent attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheBlackSerpent, EncounterSet.HaradSoldiers]
      let asideCard c = cardMatches (CardWithType ObjectiveType <> CardWithTrait Harad) c || cardMatches (cardIs Enemies.theBlackSerpent) c
          (aside, encounterDeck) = partition asideCard cards
      firstStage <- genEncounterCard Quests.theCrossroadsFort
      laterStages <- traverse genEncounterCard [Quests.battleInTheCamp, Quests.defeatTheSerpent]
      putQuestIntoPlay firstStage
      pure
        $ TheBlackSerpent
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    _ -> TheBlackSerpent <$> runMessage msg attrs
