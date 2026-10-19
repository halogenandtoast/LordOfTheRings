module LordOfTheRings.QuestAdvancementSpec (spec) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Outcome
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (encounterDiscardL, questDeckL)
import TestImport

completeQuest :: GameT ()
completeQuest = do
  quest <- getActiveQuest
  run $ PlaceTokens GameSource (toTarget quest) Progress quest.questPoints

spawn :: GameT EnemyId
spawn = getsGame (fromJustNote "no Ungoliant's Spawn" . headMay . map toId . filter ((== "01076") . toCardCode) . toList . (.entities.enemies))

-- | Skip A Fork in the Road's coin toss by leaving only the one stage after it.
nextStageIs :: CardDef -> GameT ()
nextStageIs def = do
  card <- genEncounterCard def
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL %~ \case (fork : _) -> [fork, card]; [] -> [card])

setEncounterDiscard :: [CardDef] -> GameT [EncounterCard]
setEncounterDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
  pure cards

spec :: Spec
spec = describe "Quest advancement" do
  it "replaces a completed stage with the next one, flipped and without progress" . gameTest $ \pid -> do
    flipFirstQuest pid
    completeQuest
    quest <- getActiveQuest
    toCardCode quest `shouldBe` "01120"
    quest.flipped `shouldBe` True
    quest.progress `shouldBe` 0
    getsGame (.outcome) `shouldReturn` Nothing

  it "takes one of the A Chosen Path stages at random after A Fork in the Road" . gameTest $ \pid -> do
    flipFirstQuest pid
    completeQuest
    completeQuest
    toCardCode <$> getActiveQuest >>= (`shouldSatisfy` (`elem` ["01121", "01122"]))
    getsGame (.scenario) >>= (`shouldBe` []) . (.questDeck) . toAttrs

  describe "Don't Leave the Path" do
    it "has each player add a Spider of their choice from the encounter deck or discard to staging" . gameTest $ \pid -> do
      flipFirstQuest pid
      nextStageIs Quests.aChosenPathDontLeaveThePath
      _ <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad]
      [king] <- setEncounterDiscard [Enemies.kingSpider]
      completeQuest
      completeQuest
      resolveWhenRevealed pid
      Just question <- getQuestion pid
      [() | SearchCards cards _ <- [unlabel question], length cards == 3] `shouldBe` [()]
      length [() | TargetLabel {} <- questionChoices question] `shouldBe` 2
      chooseTarget pid king
      getsGame (map toCardCode . toList . (.entities.enemies)) >>= (`shouldSatisfy` elem "01074")
      getEncounterDiscard `shouldReturn` []

    it "shows the search and carries on when there is no Spider" . gameTest $ \pid -> do
      flipFirstQuest pid
      nextStageIs Quests.aChosenPathDontLeaveThePath
      _ <- setEncounterDeck [Locations.oldForestRoad]
      _ <- setEncounterDiscard []
      completeQuest
      completeQuest
      resolveWhenRevealed pid
      chooseLabel "$label.continue" pid
      getQuestion pid `shouldReturn` Nothing

    it "is won by defeating Ungoliant's Spawn" . gameTest $ \pid -> do
      flipFirstQuest pid
      nextStageIs Quests.aChosenPathDontLeaveThePath
      [spawnCard] <- setEncounterDeck [Enemies.ungoliantsSpawn]
      completeQuest
      completeQuest
      resolveWhenRevealed pid
      chooseTarget pid spawnCard
      getsGame (.outcome) `shouldReturn` Nothing
      run . Destroy . toTarget =<< spawn
      getsGame (.outcome) `shouldReturn` Just Won

  describe "Beorn's Path" do
    it "cannot be defeated while Ungoliant's Spawn is in play, and wins once it is gone" . gameTest $ \pid -> do
      flipFirstQuest pid
      nextStageIs Quests.aChosenPathBeornsPath
      completeQuest
      completeQuest
      run . AddToStaging =<< genEncounterCard Enemies.ungoliantsSpawn
      completeQuest
      toCardCode <$> getActiveQuest `shouldReturn` "01122"
      getsGame (.outcome) `shouldReturn` Nothing
      run . Destroy . toTarget =<< spawn
      getsGame (.outcome) `shouldReturn` Just Won

  it "wins the game when the final stage is completed" . gameTest $ \pid -> do
    flipFirstQuest pid
    nextStageIs Quests.aChosenPathBeornsPath
    completeQuest
    completeQuest
    completeQuest
    getsGame (.outcome) `shouldReturn` Just Won
 where
  unlabel = \case
    QuestionLabel _ _ _ q -> unlabel q
    q -> q
