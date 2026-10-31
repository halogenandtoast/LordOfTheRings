module LordOfTheRings.Scenario.Scenarios.DeadmensDike (deadmensDike) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

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
    _ -> DeadmensDike <$> runMessage msg attrs
