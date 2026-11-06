module LordOfTheRings.Scenario.Scenarios.TheStoneOfErech (theStoneOfErech) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheStoneOfErech = TheStoneOfErech ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theStoneOfErech :: Scenario
theStoneOfErech = Scenario $ scenario TheStoneOfErech "the-stone-of-erech" "The Stone of Erech"

instance RunMessage TheStoneOfErech where
  runMessage msg (TheStoneOfErech a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.TheStoneOfErech]
      putQuestIntoPlay =<< genEncounterCard Quests.theDisappearance
      later <- traverse genEncounterCard [Quests.terrorOfTheDead, Quests.theShadowHost]
      pure $ TheStoneOfErech $ a & encounterDeckL .~ deck & questDeckL .~ later
    _ -> TheStoneOfErech <$> runMessage msg a
