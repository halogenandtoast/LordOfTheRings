module LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros (theCrossingsOfPoros) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheCrossingsOfPoros = TheCrossingsOfPoros ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCrossingsOfPoros :: Scenario
theCrossingsOfPoros = Scenario $ scenario TheCrossingsOfPoros "the-crossings-of-poros" "The Crossings of Poros"

{- | Only The Crossings of Poros set makes up the encounter deck at first. Mountains of
Shadow, Desert Sands, Desert Creatures, Harad Soldiers and Mordor Orcs are set aside, with
Poros Garrison and The Black Serpent, which stage 1A adds to the staging area (MEC61
rules). The quest deck holds every later stage, and the players advance to a stage of the
next number at random: stage 2A removes the other stage 2A, and stage 3A sets the others
aside, so the next stage is drawn at random from the lowest number left.
-}
instance RunMessage TheCrossingsOfPoros where
  runMessage msg (TheCrossingsOfPoros attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheCrossingsOfPoros]
      aside <-
        gatherEncounterSets
          attrs
          [EncounterSet.MountainsOfShadow, EncounterSet.DesertSands, EncounterSet.DesertCreatures, EncounterSet.HaradSoldiers, EncounterSet.MordorOrcs]
      let (setUp, encounterDeck) =
            partition (cardMatches $ CardMatchAny [cardIs Objectives.porosGarrison, cardIs Enemies.theBlackSerpentCrossingsOfPoros]) cards
      firstStage <- genEncounterCard Quests.flightToGondor
      laterStages <-
        traverse
          genEncounterCard
          [ Quests.theDesertPath
          , Quests.theEphelDuath
          , Quests.deadlyCreatures
          , Quests.southronPursuit
          , Quests.orcPatrols
          , Quests.battleAtTheCrossings
          ]
      putQuestIntoPlay firstStage
      pure
        $ TheCrossingsOfPoros
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ setUp <> aside
    AdvanceQuest | Just lowest <- minimumMay (map stageOf attrs.questDeck) -> do
      next <- uniform (filter ((== lowest) . stageOf) attrs.questDeck)
      advanceToQuest next
      pure $ TheCrossingsOfPoros $ attrs & questDeckL %~ filter (/= next)
    _ -> TheCrossingsOfPoros <$> runMessage msg attrs
   where
    stageOf = fromMaybe 1 . cdStage . toCardDef
