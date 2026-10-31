module LordOfTheRings.Scenario.Scenarios.IntrudersInChetwood (intrudersInChetwood) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (iarionsPendantOnIarion, protectTheInnocentSetup, recordProtectTheInnocent)

newtype IntrudersInChetwood = IntrudersInChetwood ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intrudersInChetwood :: Scenario
intrudersInChetwood = Scenario $ scenario IntrudersInChetwood "intruders-in-chetwood" "Intruders in Chetwood"

-- | Built from Intruders in Chetwood, Eriador Wilds and Angmar Orcs, with Iârion from The
-- Lost Realm set, whom the stage's setup takes out again. One stage.
instance RunMessage IntrudersInChetwood where
  runMessage msg (IntrudersInChetwood attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.IntrudersInChetwood, EncounterSet.EriadorWilds, EncounterSet.AngmarOrcs, EncounterSet.TheLostRealm]
      putQuestIntoPlay =<< genEncounterCard Quests.stopTheWarParty
      pure $ IntrudersInChetwood $ attrs & encounterDeckL .~ encounterDeck
    -- Campaign: Setup: Attach Iârion's Pendant to Iârion and add the Protect the Innocent
    -- side quest to the staging area. Starting with the first player, each player may place
    -- 4 damage on Protect the Innocent to add 1 resource to each of their heroes' resource
    -- pools. (Iârion is in play once the stage has set up.)
    DoStep 9 Setup | Just l <- attrs.campaign -> do
      iarionsPendantOnIarion
      protectTheInnocentSetup attrs l True
      IntrudersInChetwood <$> runMessage msg attrs
    -- Resolution: Note the amount of damage on Protect the Innocent in the Campaign Log and
    -- add Protect the Innocent to the Campaign Pool: the later campaign cards set it up from
    -- the number noted.
    ResolveCampaign | Just l <- attrs.campaign -> do
      l' <- recordProtectTheInnocent l
      IntrudersInChetwood <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> IntrudersInChetwood <$> runMessage msg attrs
