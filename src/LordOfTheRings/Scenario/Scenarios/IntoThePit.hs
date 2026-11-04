module LordOfTheRings.Scenario.Scenarios.IntoThePit (intoThePit) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ability (forced, mkAbility)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (caveTorchExhausts)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype IntoThePit = IntoThePit ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoThePit :: Scenario
intoThePit = Scenario $ scenario (IntoThePit . (nightmareDeckL ?~ nightmare)) "into-the-pit" "Into the Pit"
 where
  -- Goblin Ambush takes Goblin Patrol's place in the quest deck
  nightmare =
    nightmareDeck
      Nightmare.intoThePitNightmare
      EncounterSet.IntoThePitNightmare
      [ (Enemies.goblinSwordsman, 5)
      , (Locations.goblinTunnels, 2)
      , (Treacheries.caveIn, 3)
      , (Treacheries.crumblingRuin, 2)
      , (Locations.fouledWell, 2)
      , (Locations.stairsOfNain, 2)
      , (Enemies.goblinFollower, 2)
      ]

-- | Nightmare: "Forced: After Cave Torch exhausts, place 1 damage token on this card.
-- Then, if there are 5 or more damage tokens on this card, remove Cave Torch from the
-- game."
instance HasAbilities IntoThePit where
  getAbilities (IntoThePit a) = [mkAbility a 1 $ forced caveTorchExhausts | a.nightmare]

instance RunMessage IntoThePit where
  runMessage msg (IntoThePit attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.IntoThePit, EncounterSet.HazardsOfThePit, EncounterSet.TwistsAndTurns, EncounterSet.GoblinsOfTheDeep]
      firstStage <- genEncounterCard Quests.enteringTheMines
      laterStages <- traverse genEncounterCard [nightmareOr attrs Quests.goblinPatrol Quests.goblinAmbush, Quests.aWayUp]
      putQuestIntoPlay firstStage
      pure $ IntoThePit $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    UseCardAbility _ ScenarioSource 1 _ -> do
      let damage = 1 + fromMaybe (0 :: Int) (parseMaybe parseJSON attrs.scenarioMeta)
      when (damage >= 5) do
        torches <- getsGame (filter ((== toCardCode Objectives.caveTorch) . toCardCode) . toList . (.entities.objectives))
        for_ torches removeFromGame
      pure $ IntoThePit $ attrs & metaL .~ toJSON damage
    _ -> IntoThePit <$> runMessage msg attrs
