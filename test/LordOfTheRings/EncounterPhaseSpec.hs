module LordOfTheRings.EncounterPhaseSpec (spec) where

import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import LordOfTheRings.Window (WindowType (EnemyEngaged, ThreatRaised), mkWhen)
import TestImport

enemyPlacements :: GameT [(CardCode, Placement)]
enemyPlacements = getsGame (map (\e -> (toCardCode e, enemyPlacement $ toAttrs e)) . toList . (.entities.enemies))

spec :: Spec
spec = describe "Encounter phase" do
  it "opens a window before an enemy engages, while it is still in the staging area" . gameTest $ \pid -> do
    card <- genEncounterCard Enemies.forestSpider
    run $ AddToStaging card
    eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
    runOnly $ EngageEnemy pid eid
    headMay <$> peekQueue `shouldReturn` Just (CheckWindows [mkWhen $ EnemyEngaged eid pid])
    enemyPlacements `shouldReturn` [("01096", StagingArea)]

  it "has one enemy make engagement checks alone, as False Accusations' shadow reads" . gameTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    run . AddToStaging =<< genEncounterCard Enemies.kingSpider
    spider <- getsGame (fromJustNote "no spider" . headMay . map toId . filter ((== toCardCode Enemies.forestSpider) . toCardCode) . toList . (.entities.enemies))
    -- the player's threat reaches both enemies' engagement costs
    threat <- (.threat) <$> getPlayer pid
    run $ RaiseThreat pid (max 0 (40 - threat))
    run $ EnemyEngagementChecks spider
    sort <$> enemyPlacements `shouldReturn` sort [(toCardCode Enemies.forestSpider, EngagedWith pid), (toCardCode Enemies.kingSpider, StagingArea)]

  it "opens a window before a player's threat is raised" . gameTest $ \pid -> do
    was <- (.threat) <$> getPlayer pid
    runOnly $ RaiseThreat pid 2
    headMay <$> peekQueue `shouldReturn` Just (CheckWindows [mkWhen $ ThreatRaised pid])
    (.threat) <$> getPlayer pid `shouldReturn` was
    void mainLoop
    (.threat) <$> getPlayer pid `shouldReturn` was + 2

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
    resolveRevealed
    run RevealEncounterCard
    resolveRevealed
    run $ BeginPhase EncounterPhase
    chooseLabel "$label.doNotEngage" pid
    -- the action window before the engagement checks
    passActions pid
    placements <- enemyPlacements
    sort placements
      `shouldBe` sort [("01096", EngagedWith pid), ("01098", EngagedWith pid), ("01076", StagingArea)]
