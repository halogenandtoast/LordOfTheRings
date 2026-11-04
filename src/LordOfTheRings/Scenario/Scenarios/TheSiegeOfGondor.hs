module LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor (theSiegeOfGondor) where

import LordOfTheRings.Campaign (CampaignLogKey (CorsairFleetResources), CampaignLogUpdate (RemoveFromPool), recordCount, updateLog)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (mayRaiseThreatToSearchForAnduril)
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (corsairFleetResources, runSiegeOfGondorStep)

newtype TheSiegeOfGondor = TheSiegeOfGondor ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSiegeOfGondor :: Scenario
theSiegeOfGondor = Scenario $ scenario TheSiegeOfGondor "the-siege-of-gondor" "The Siege of Gondor"

{- | The Siege of Gondor's encounter set makes up the encounter deck (MEC54 rules). The
Corsair Fleet and Great Corsair Ship are set aside for stage 1A to add.

The first player takes control of the Fellowship sphere Aragorn as the game sets up (see
'LordOfTheRings.Saga.fellowshipSetup'). In campaign mode an earned Army of the Dead comes
into play under the first player's control (its "Setup" text, see
'LordOfTheRings.Scenario.Runner.entersPlayAtSetup').
-}
instance RunMessage TheSiegeOfGondor where
  runMessage msg (TheSiegeOfGondor attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheSiegeOfGondor]
      let aside c = toCardCode c `elem` map toCardCode [Objectives.theCorsairFleet, Locations.greatCorsairShip]
          (setAside', deck) = partition aside cards
      putQuestIntoPlay =<< genEncounterCard Quests.attackOnPelargir
      pure
        $ TheSiegeOfGondor
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
    -- Campaign: Setup: The first player may raise each player's threat by 1 to search his
    -- deck for Andúril and add it to his hand. Remove each burden with the following burden
    -- set icons from the encounter deck (see 'LordOfTheRings.Saga.removedBurdenSets').
    DoStep 9 Setup | isJust attrs.campaign -> do
      mayRaiseThreatToSearchForAnduril attrs
      TheSiegeOfGondor <$> runMessage msg attrs
    -- Resolution: Count the number of resources on The Corsair Fleet and record that number
    -- in the notes section of the quest log. Remove Army of the Dead from the campaign pool.
    ResolveCampaign | Just l <- attrs.campaign -> do
      n <- corsairFleetResources
      let l' = updateLog (RemoveFromPool $ toCardCode Objectives.armyOfTheDeadBoon) $ recordCount CorsairFleetResources (toCardCode Objectives.theCorsairFleet) n l
      TheSiegeOfGondor <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    ScenarioSpecific key v -> do
      runSiegeOfGondorStep key v
      pure $ TheSiegeOfGondor attrs
    _ -> TheSiegeOfGondor <$> runMessage msg attrs
