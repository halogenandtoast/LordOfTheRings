module LordOfTheRings.Scenario.Scenarios.FireInTheNight (fireInTheNight) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Types (isEncounterSideQuest)
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (sideQuestDeckOf)

newtype FireInTheNight = FireInTheNight ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fireInTheNight :: Scenario
fireInTheNight = Scenario $ scenario FireInTheNight "fire-in-the-night" "Fire in the Night"

{- | The encounter deck is built from Fire in the Night, Grey Mountain Goblins, Dark Woods
and Gathering Gloom. Stage 1A's setup searches it for the 8 encounter side quests,
shuffled together facedown as the side quest deck: the scenario takes them out as it
builds the deck, keeping the side quest deck in its meta, and stage 1B reveals its top
card at the end of each planning phase. Side quests are never part of the quest deck
(MEC68 rules, Side Quests).
-}
instance RunMessage FireInTheNight where
  runMessage msg (FireInTheNight attrs) = case msg of
    DoStep 1 Setup -> do
      gathered <-
        gatherEncounterSets attrs [EncounterSet.FireInTheNight, EncounterSet.GreyMountainGoblins, EncounterSet.DarkWoods, EncounterSet.GatheringGloom]
      let (sideQuests, encounterDeck) = partition isEncounterSideQuest gathered
      sideQuestDeck <- shuffleM sideQuests
      putQuestIntoPlay =<< genEncounterCard Quests.hrogarsHill
      pure
        $ FireInTheNight
        $ attrs
        & encounterDeckL .~ encounterDeck
        & metaL .~ object ["sideQuestDeck" .= sideQuestDeck]
    ScenarioSpecific "fireInTheNight.revealSideQuest" _ -> case sideQuestDeckOf attrs.meta of
      [] -> pure $ FireInTheNight attrs
      top : rest -> do
        revealSetAsideCard top
        pure $ FireInTheNight $ attrs & metaL .~ object ["sideQuestDeck" .= rest]
    _ -> FireInTheNight <$> runMessage msg attrs
