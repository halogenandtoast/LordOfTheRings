module LordOfTheRings.Core.JourneyAlongTheAnduinSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import TestImport

journeyTest :: (PlayerId -> GameT ()) -> IO ()
journeyTest = gameTestIn journeyAlongTheAnduin starterDeck

enemiesWith :: CardDef -> GameT [EnemyId]
enemiesWith def = getsGame (map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

spec :: Spec
spec = describe "Journey Along the Anduin" do
  it "sets up To the River... with a Hill Troll in the staging area" . journeyTest $ \pid -> do
    run Setup
    chooseLabel "Keep this hand" pid
    chooseOptionMatching "flip the quest" pid (const True)
    length <$> enemiesWith Enemies.hillTroll `shouldReturn` 1
    (.flipped) <$> getActiveQuest `shouldReturn` True

  it "cannot leave To the River... while a Hill Troll is in play" . journeyTest $ \pid -> do
    run Setup
    chooseLabel "Keep this hand" pid
    chooseOptionMatching "flip the quest" pid (const True)
    quest <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource quest Progress 20
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.toTheRiver
    [troll] <- enemiesWith Enemies.hillTroll
    run $ Discard (toTarget troll)
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.anduinPassage

  it "reveals an extra card in staging and makes no engagement checks at Anduin Passage" . journeyTest $ \pid -> do
    _ <- putQuestStage Quests.anduinPassage
    _ <- setEncounterDeck [Enemies.forestSpider, Locations.forestGate]
    run $ BeginPhase QuestPhase
    chooseLabel "Done committing" pid
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    run $ EngagementCheck pid 0
    spider <- fromJustNote "no spider" . headMay <$> enemiesWith Enemies.forestSpider
    getsGame (fmap (.placement) . lookup spider . mapToList . (.entities.enemies)) `shouldReturn` Just StagingArea

  it "wins at Ambush on the Shore once no enemies are in play" . journeyTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.forestSpider, Enemies.forestSpider]
    stage <- putQuestStage Quests.ambushOnTheShore
    run $ Flip stage
    spiders <- enemiesWith Enemies.forestSpider
    length spiders `shouldBe` 2
    for_ spiders \eid -> run $ DealDamage GameSource (toTarget eid) 4
    getsGame (.outcome) `shouldReturn` Just Won

  it "skips the staging step at Ambush on the Shore" . journeyTest $ \pid -> do
    stage <- putQuestStage Quests.ambushOnTheShore
    _ <- setEncounterDeck [Enemies.forestSpider, Enemies.forestSpider, Locations.forestGate]
    run $ Flip stage
    run $ BeginPhase QuestPhase
    chooseLabel "Done committing" pid
    getsGame (length . (.entities.locations)) `shouldReturn` 0
