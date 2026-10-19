module LordOfTheRings.EncounterPhaseSpec (spec) where

import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import TestImport

enemyPlacements :: GameT [(CardCode, Placement)]
enemyPlacements = getsGame (map (\e -> (toCardCode e, enemyPlacement $ toAttrs e)) . toList . (.entities.enemies))

spec :: Spec
spec = describe "Encounter phase" do
  it "offers every staging enemy for optional engagement" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase EncounterPhase
    Just question <- getQuestion pid
    length [() | TargetLabel {} <- questionChoices question] `shouldBe` 1

  it "puts an engaged enemy in front of the player" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase EncounterPhase
    chooseOptionMatching "engage" pid \case
      TargetLabel {} -> True
      _ -> False
    enemyPlacements `shouldReturn` [("01096", EngagedWith pid)]

  it "engages the highest cost enemy the player's threat has reached" . gameTest $ \pid -> do
    flipFirstQuest pid
    -- threat 27: Black Forest Bats (15) and Forest Spider (25) can engage, Ungoliant's Spawn (32) cannot
    _ <- setEncounterDeck [Enemies.blackForestBats, Enemies.ungoliantsSpawn]
    run RevealEncounterCard
    run RevealEncounterCard
    run $ BeginPhase EncounterPhase
    chooseLabel "Do not engage" pid
    placements <- enemyPlacements
    sort placements
      `shouldBe` sort [("01096", EngagedWith pid), ("01098", EngagedWith pid), ("01076", StagingArea)]
