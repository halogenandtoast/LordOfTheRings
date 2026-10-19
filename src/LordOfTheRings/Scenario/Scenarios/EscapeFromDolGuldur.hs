module LordOfTheRings.Scenario.Scenarios.EscapeFromDolGuldur (escapeFromDolGuldur) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype EscapeFromDolGuldur = EscapeFromDolGuldur ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromDolGuldur :: Scenario
escapeFromDolGuldur = Scenario $ scenario EscapeFromDolGuldur "escape-from-dol-guldur" "Escape from Dol Guldur"

instance RunMessage EscapeFromDolGuldur where
  runMessage msg (EscapeFromDolGuldur attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          gatherEncounterSet
          [EncounterSet.EscapeFromDolGuldur, EncounterSet.SpidersOfMirkwood, EncounterSet.DolGuldurOrcs]
      pure $ EscapeFromDolGuldur $ attrs & encounterDeckL .~ encounterDeck
    DoStep 6 Setup -> do
      firstStage <- genEncounterCard Quests.theNecromancersTower
      laterStages <- traverse genEncounterCard [Quests.throughTheCaverns, Quests.outOfTheDungeons]
      push $ PutQuestIntoPlay firstStage
      pure $ EscapeFromDolGuldur $ attrs & questDeckL .~ laterStages
    -- Nazgul of Dol Guldur: Forced: When the prisoner is "rescued", move Nazgul of Dol
    -- Guldur into the staging area.
    RescuePrisoner _ -> do
      let (nazgul, rest) = partition ((== toCardCode Enemies.nazgulOfDolGuldur) . toCardCode) attrs.scenarioSetAside
      pushAll $ map AddToStaging nazgul
      pure $ EscapeFromDolGuldur $ attrs & setAsideL .~ rest
    _ -> EscapeFromDolGuldur <$> runMessage msg attrs
