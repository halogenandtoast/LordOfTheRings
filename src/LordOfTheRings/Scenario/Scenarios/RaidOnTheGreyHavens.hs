module LordOfTheRings.Scenario.Scenarios.RaidOnTheGreyHavens (raidOnTheGreyHavens) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype RaidOnTheGreyHavens = RaidOnTheGreyHavens ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

raidOnTheGreyHavens :: Scenario
raidOnTheGreyHavens = Scenario $ scenario RaidOnTheGreyHavens "raid-on-the-grey-havens" "Raid on the Grey Havens"

{- | The encounter deck is built from the Raid on the Grey Havens and Corsair Raiders sets.
Captain Sahír and Na'asiyah of The Stormcaller Elite are set aside, enemy side faceup, for
Sahír's Advance to add to the staging area. There is no Corsair deck and no fleet: the
Dream-chaser is a location here.
-}
instance RunMessage RaidOnTheGreyHavens where
  runMessage msg (RaidOnTheGreyHavens attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- concatMapM (gatherEncounterSet attrs) [EncounterSet.RaidOnTheGreyHavens, EncounterSet.CorsairRaiders]
      elite <- traverse genEncounterCard [Enemies.captainSahir, Enemies.naasiyah]
      firstStage <- genEncounterCard Quests.theCorsairsAssault
      laterStages <- traverse genEncounterCard [Quests.sahirsAdvance]
      putQuestIntoPlay firstStage
      pure
        $ RaidOnTheGreyHavens
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ elite
    _ -> RaidOnTheGreyHavens <$> runMessage msg attrs
