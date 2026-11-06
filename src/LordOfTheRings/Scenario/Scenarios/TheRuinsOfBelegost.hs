module LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost (theRuinsOfBelegost) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (heroHasLoot)

newtype TheRuinsOfBelegost = TheRuinsOfBelegost ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRuinsOfBelegost :: Scenario
theRuinsOfBelegost = Scenario $ scenario TheRuinsOfBelegost "the-ruins-of-belegost" "The Ruins of Belegost"

instance RunMessage TheRuinsOfBelegost where
  runMessage msg (TheRuinsOfBelegost a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.TheRuinsOfBelegost]
      putQuestIntoPlay =<< genEncounterCard Quests.ruinsOfTheFirstAge
      later <- traverse genEncounterCard [Quests.ancientEvilsDisturbed, Quests.deeperIntoThePast, Quests.theServantsOfMorgoth, Quests.theRootsOfEredLuin, Quests.theMinesOfBelegost, Quests.theBeastOfBelegost]
      pure $ TheRuinsOfBelegost $ a & encounterDeckL .~ deck & questDeckL .~ later
    CompleteQuest qid -> do
      current <- getsGame (lookup qid . (.entities.quests))
      case current of
        Nothing -> TheRuinsOfBelegost <$> runMessage msg a
        Just q | toCardCode q `elem` map toCardCode [Quests.ruinsOfTheFirstAge, Quests.ancientEvilsDisturbed, Quests.deeperIntoThePast, Quests.theServantsOfMorgoth, Quests.theRootsOfEredLuin, Quests.theMinesOfBelegost] -> do
          let code = toCardCode q
              chooseBy loot yes no = do
                held <- heroHasLoot loot
                pure $ if held then yes else no
          next <- if code == toCardCode Quests.ruinsOfTheFirstAge then chooseBy Objectives.keysOfBelegost Quests.deeperIntoThePast Quests.ancientEvilsDisturbed
            else if code == toCardCode Quests.ancientEvilsDisturbed then chooseBy Objectives.bookOfAulesChildren Quests.theRootsOfEredLuin Quests.theServantsOfMorgoth
            else if code == toCardCode Quests.deeperIntoThePast then chooseBy Objectives.bookOfAulesChildren Quests.theMinesOfBelegost Quests.theRootsOfEredLuin
            else if code == toCardCode Quests.theServantsOfMorgoth then chooseBy Objectives.dwarvenTorch Quests.theBeastOfBelegost Quests.theRootsOfEredLuin
            else if code == toCardCode Quests.theRootsOfEredLuin then chooseBy Objectives.mapOfEredLuin Quests.theBeastOfBelegost Quests.theMinesOfBelegost
            else chooseBy Objectives.blueMountainGem Quests.theBeastOfBelegost Quests.theServantsOfMorgoth
          let deck = a.questDeck
              chosen = filter ((== toCardCode next) . toCardCode) deck
              remaining = filter ((/= toCardCode next) . toCardCode) deck
              reusable = [MkEncounterCard (toAttrs q).cardId (toCardCode q) | code `elem` map toCardCode [Quests.theServantsOfMorgoth, Quests.theRootsOfEredLuin, Quests.theMinesOfBelegost]]
          -- The scenario chooses the next physical stage before the normal
          -- CompleteQuest removes the old one and queues AdvanceQuest.
          pure $ TheRuinsOfBelegost $ a & questDeckL .~ (chosen <> remaining <> reusable)
        _ -> TheRuinsOfBelegost <$> runMessage msg a
    _ -> TheRuinsOfBelegost <$> runMessage msg a
