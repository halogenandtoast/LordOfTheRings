module LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven (aStormOnCobasHaven) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign)

newtype AStormOnCobasHaven = AStormOnCobasHaven ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aStormOnCobasHaven :: Scenario
aStormOnCobasHaven = Scenario $ scenario AStormOnCobasHaven "a-storm-on-cobas-haven" "A Storm on Cobas Haven"

{- | The encounter deck is built from the A Storm on Cobas Haven, Voyage Across Belegaer,
Umbar Navy, Corsair Pirates and Corsair Raiders sets; The Dream-chaser's Fleet is prepared
by the first stage. Each copy of Dol Amroth Warship is set aside, out of play.
-}
instance RunMessage AStormOnCobasHaven where
  runMessage msg (AStormOnCobasHaven attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
          [ EncounterSet.AStormOnCobasHaven
          , EncounterSet.VoyageAcrossBelegaer
          , EncounterSet.UmbarNavy
          , EncounterSet.CorsairPirates
          , EncounterSet.CorsairRaiders
          ]
      let (warships, encounterDeck) = partition (cardMatches $ cardIs Objectives.dolAmrothWarship) cards
      firstStage <- genEncounterCard Quests.outmaneuverTheEnemy
      laterStages <- traverse genEncounterCard [Quests.battleInTheBay, Quests.breakThroughTheFleet]
      putQuestIntoPlay firstStage
      pure
        $ AStormOnCobasHaven
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ warships
    -- Campaign: the Dream-chaser campaign card's setup, Forced effects and resolution
    _ | isJust attrs.campaign -> AStormOnCobasHaven <$> runDreamChaserCampaign msg attrs
    _ -> AStormOnCobasHaven <$> runMessage msg attrs
