module LordOfTheRings.Scenario.Scenarios.DeadmensDike (deadmensDike) where

import LordOfTheRings.Campaign
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (iarionsPendantOnIarion, protectTheInnocentSetup, recordProtectTheInnocent, thaurdirsBurdens)

newtype DeadmensDike = DeadmensDike ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadmensDike :: Scenario
deadmensDike = Scenario $ scenario DeadmensDike "deadmens-dike" "Deadmen's Dike"

-- | Deadmen's Dike, Cursed Dead, Dark Sorcery, Ruins of Arnor and The Lost Realm (Iârion)
-- make up the encounter deck. A player with no cards in his deck is eliminated: see the
-- stages.
instance RunMessage DeadmensDike where
  runMessage msg (DeadmensDike attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [ EncounterSet.DeadmensDike
          , EncounterSet.CursedDead
          , EncounterSet.DarkSorcery
          , EncounterSet.RuinsOfArnor
          , EncounterSet.TheLostRealm
          ]
      firstStage <- genEncounterCard Quests.theShadesOfAngmar
      laterStages <- traverse genEncounterCard [Quests.aFellWraith]
      putQuestIntoPlay firstStage
      pure $ DeadmensDike $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Campaign: Setup: Attach Iârion's Pendant to Iârion. Add the Protect the Innocent side
    -- quest to the staging area and place damage on it equal to the amount of damage noted
    -- in the Campaign Log.
    DoStep 9 Setup | Just l <- attrs.campaign -> do
      iarionsPendantOnIarion
      protectTheInnocentSetup attrs l False
      DeadmensDike <$> runMessage msg attrs
    -- Forced: After Thaurdir enters play, randomly select one of the following burdens and
    -- attach it to Thaurdir: Thaurdir's Legacy, Thaurdir's Majesty, or Thaurdir's Spite. Add
    -- that card to the Campaign Pool.
    CheckWindows _ | isJust attrs.campaign -> do
      thaurdirsBurdens attrs True
      DeadmensDike <$> runMessage msg attrs
    -- Resolution: Note the amount of damage on Protect the Innocent in the Campaign Log
    -- (replacing the previous number) and remove Protect the Innocent from the Campaign
    -- Pool. Add Arnor Ravaged to the Campaign Pool in its place. The players as a group must
    -- select either Iârion's Pendant or Amarthiúl's Courage to add to the Campaign Pool:
    -- the game being over, they do as The Wastes of Eriador is set up.
    ResolveCampaign | Just l <- attrs.campaign -> do
      l' <- recordProtectTheInnocent l
      let l'' = record ChoosePendantOrCourage [] $ addToPool (PoolCard (toCardCode Objectives.arnorRavaged) Nothing Nothing) l'
      DeadmensDike <$> runMessage msg (attrs {scenarioCampaign = Just l''})
    _ -> DeadmensDike <$> runMessage msg attrs
