module LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim (overTheMistyMountainsGrim) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype OverTheMistyMountainsGrim = OverTheMistyMountainsGrim ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

overTheMistyMountainsGrim :: Scenario
overTheMistyMountainsGrim = Scenario $ scenario OverTheMistyMountainsGrim "over-the-misty-mountains-grim" "Over the Misty Mountains Grim"

-- | The cards of the second encounter deck: The Great Goblin and Misty Mountain Goblins sets.
inSecondDeck :: EncounterCard -> Bool
inSecondDeck = (`elem` map Just [EncounterSet.TheGreatGoblin, EncounterSet.MistyMountainGoblins]) . cdEncounterSet . toCardDef

{- | Played with two encounter decks (rules insert, "Active and Inactive Encounter Decks"):
the Over the Misty Mountains Grim and Western Lands sets make up the active encounter
deck, The Great Goblin and Misty Mountain Goblins sets the second, set aside inactive
until Down, Down to Goblin Town makes it the active one. Stage 1A's setup is this.
-}
instance RunMessage OverTheMistyMountainsGrim where
  runMessage msg (OverTheMistyMountainsGrim attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.OverTheMistyMountainsGrim
          , EncounterSet.WesternLands
          , EncounterSet.TheGreatGoblin
          , EncounterSet.MistyMountainGoblins
          ]
      let (secondDeck, encounterDeck) = partition inSecondDeck cards
      firstStage <- genEncounterCard Quests.aShortRest
      laterStages <- traverse genEncounterCard [Quests.theMountainPass, Quests.downDownToGoblinTown]
      putQuestIntoPlay firstStage
      pure
        $ OverTheMistyMountainsGrim
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ secondDeck
    -- The first deck, the encounter cards in play shuffled into it, and its discard pile
    -- are set aside inactive. Nothing makes them active again, so they are let go rather
    -- than kept with the set-aside cards, where effects reading set-aside cards would find
    -- them.
    ScenarioSpecific "overTheMistyMountainsGrim.switchEncounterDeck" _ -> do
      deck <- shuffleM attrs.scenarioSetAside
      pure
        $ OverTheMistyMountainsGrim
        $ attrs
        & encounterDeckL .~ deck
        & encounterDiscardL .~ []
        & setAsideL .~ []
    _ -> OverTheMistyMountainsGrim <$> runMessage msg attrs
