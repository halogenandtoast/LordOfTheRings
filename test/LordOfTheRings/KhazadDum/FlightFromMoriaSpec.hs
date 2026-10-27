module LordOfTheRings.KhazadDum.FlightFromMoriaSpec (spec) where

import LordOfTheRings.Card (CardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (flightFromMoria)
import LordOfTheRings.Scenario.Types (questDeckL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds, StagingStepBegins), mkAfter, mkWhen)
import TestImport

flightTest :: (PlayerId -> GameT ()) -> IO ()
flightTest = gameTestIn flightFromMoria testDeck

codes :: [CardDef] -> [CardCode]
codes = map toCardCode

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

questDeck :: GameT [CardCode]
questDeck = getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

addToVictory :: CardDef -> GameT ()
addToVictory def = do
  card <- EncounterCard <$> genEncounterCard def
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (victoryDisplayL %~ (card :))

-- | The stage put into play, and flipped as the staging step begins.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  stage <- putQuestStage def
  run $ CheckWindows [mkAfter StagingStepBegins]
  pure stage

activeQuest :: GameT (CardCode, Bool)
activeQuest = (\q -> (toCardCode q, (toAttrs q).questFlipped)) <$> getActiveQuest

spec :: Spec
spec = describe "Flight from Moria" do
  it "keeps 1 A Foe Beyond per player, and shuffles the 7 Search for an Exit stages" . flightTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    deck <- getEncounterDeck
    length (filter ((== toCardCode Treacheries.aFoeBeyond) . toCardCode) deck) `shouldBe` 1
    length <$> questDeck `shouldReturn` 7

  it "adds The Nameless Fear to staging, and leaves A Presence in the Dark for a face down stage" . flightTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    _ <- setEncounterDeck [Enemies.theNamelessFear, Enemies.strayGoblin]
    chooseOptionMatching "flip the quest" pid (const True)
    getsGame (sort . map toCardCode . toList . (.entities.enemies)) `shouldReturn` sort (codes [Enemies.theNamelessFear, Enemies.strayGoblin])
    victoryDisplay `shouldReturn` codes [Quests.aPresenceInTheDark]
    (.stage) <$> getActiveQuest `shouldReturn` 2
    (toAttrs <$> getActiveQuest) >>= (`shouldBe` False) . (.questFlipped)
    -- X is the 2 victory points of A Presence in the Dark
    (getEnemyAttack =<< enemyOf Enemies.theNamelessFear) `shouldReturn` 2

  it "flips a Search for an Exit stage at the staging step, ignoring progress before" . flightTest $ \_ -> do
    stage <- putQuestStage Quests.aWrongTurn
    run $ PlaceTokens GameSource stage Progress 3
    (.progress) <$> getActiveQuest `shouldReturn` 0
    _ <- setEncounterDeck [Enemies.strayGoblin]
    run $ CheckWindows [mkAfter StagingStepBegins]
    activeQuest `shouldReturn` (toCardCode Quests.aWrongTurn, True)
    getsGame (length . (.entities.enemies)) `shouldReturn` 1

  it "bypasses the stage at the end of combat, to the bottom of the quest deck" . flightTest $ \pid -> do
    stage <- flippedStage Quests.headingDown
    setQuestDeck [Quests.headingUp]
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    chooseAbilityOf pid stage
    activeQuest `shouldReturn` (toCardCode Quests.headingUp, False)
    questDeck `shouldReturn` codes [Quests.headingDown]

  it "shuffles Heading Up back from the victory display as the quest phase ends" . flightTest $ \_ -> do
    _ <- flippedStage Quests.headingDown
    addToVictory Quests.headingUp
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    questDeck `shouldReturn` codes [Quests.headingUp]
    victoryDisplay `shouldReturn` []

  it "puts a defeated stage in the victory display, and advances" . flightTest $ \_ -> do
    stage <- flippedStage Quests.aWrongTurn
    setQuestDeck [Quests.headingUp]
    run $ PlaceTokens GameSource stage Progress 1
    victoryDisplay `shouldReturn` codes [Quests.aWrongTurn]
    activeQuest `shouldReturn` (toCardCode Quests.headingUp, False)

  it "has New Devilry shuffle the stage back and reveal another" . flightTest $ \_ -> do
    _ <- flippedStage Quests.headingDown
    setQuestDeck [Quests.headingUp]
    _ <- setEncounterDeck [Treacheries.newDevilry]
    run RevealEncounterCard
    resolveRevealed
    -- the stage is shuffled in, so either may come out, face down
    (active, flipped) <- activeQuest
    flipped `shouldBe` False
    sort . (active :) <$> questDeck `shouldReturn` sort (codes [Quests.headingDown, Quests.headingUp])

  it "has Hasty Council make 1 of the top 2 stages active, flipped" . flightTest $ \pid -> do
    stage <- putQuestStage Quests.hastyCouncil
    setQuestDeck [Quests.headingDown, Quests.headingUp]
    run $ Flip stage
    up <- getsGame (fromJustNote "no Heading Up" . find ((== toCardCode Quests.headingUp) . toCardCode) . (.questDeck) . toAttrs . (.scenario))
    chooseTarget pid up
    activeQuest `shouldReturn` (toCardCode Quests.headingUp, True)
    questDeck `shouldReturn` codes [Quests.headingDown]
    victoryDisplay `shouldReturn` codes [Quests.hastyCouncil]

  it "wins when Blocked by Shadow is defeated" . flightTest $ \pid -> do
    stage <- putQuestStage Quests.blockedByShadow
    setQuestDeck [Quests.headingUp]
    run $ Flip stage
    _ <- setEncounterDeck [Enemies.strayGoblin]
    chooseLabel "$label.blockedByShadow.discard" pid
    getsGame (.outcome) `shouldReturn` Nothing
    run $ PlaceTokens GameSource stage Progress 9
    getsGame (.outcome) `shouldReturn` Just Won

  it "eliminates a player who discards a treachery at Blocked by Shadow" . flightTest $ \pid -> do
    stage <- putQuestStage Quests.blockedByShadow
    run $ Flip stage
    _ <- setEncounterDeck [Treacheries.newDevilry]
    chooseLabel "$label.blockedByShadow.discard" pid
    getsGame (.outcome) `shouldReturn` Just Lost

  it "takes progress at Escape from Darkness only from Abandoned Tools, and wins" . flightTest $ \pid -> do
    stage <- flippedStage Quests.escapeFromDarkness
    run $ PlaceTokens GameSource stage Progress 4
    (.progress) <$> getActiveQuest `shouldReturn` 0
    beravor <- toTarget <$> getHeroWithCode "01012"
    run . (`PutObjectiveIntoPlayAttached` beravor) =<< genEncounterCard Objectives.abandonedTools
    run $ BeginPhase RefreshPhase
    chooseAbilityOf pid stage
    getsGame (.outcome) `shouldReturn` Nothing
    (.progress) <$> getActiveQuest `shouldReturn` 1

  it "has Chance Encounter engage the top enemy of the discard pile with the first player" . flightTest $ \pid -> do
    goblin <- genEncounterCard Enemies.strayGoblin
    run $ AddToEncounterDiscard [goblin]
    _ <- setEncounterDeck [Treacheries.chanceEncounter]
    run RevealEncounterCard
    resolveRevealed
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [EngagedWith pid]

  it "gives The Mountains' Roots and Stray Goblin X for the players" . flightTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.strayGoblin, Locations.theMountainsRoots]
    run RevealEncounterCard
    run RevealEncounterCard
    (getEnemyAttack =<< enemyOf Enemies.strayGoblin) `shouldReturn` 1
