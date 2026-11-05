module LordOfTheRings.MountainOfFire.MountDoomSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Campaign (CampaignLogKey (..), CompletedScenario (..), newCampaignLog, nextScenario, recordCount)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (PlayerAtValourThreat), getEliminationLevel, selectAny)
import LordOfTheRings.Message.Lifted (failedFortitudeTestThisRound, lowerThreatBy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, mountDoom)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

doomTest :: (PlayerId -> GameT ()) -> IO ()
doomTest = gameTestIn mountDoom testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

beravor :: CardCode
beravor = "01012"

frodo :: CardCode
frodo = toCardCode Heroes.frodoBagginsTheMountainOfFire

-- | Answer the first choice of each question until none is left.
answerAll :: PlayerId -> GameT ()
answerAll pid = getQuestion pid >>= traverse_ \_ -> answerQuestion pid 0 >> answerAll pid

-- | Answer until the Fortitude test asks for heroes.
answerAll' :: PlayerId -> GameT ()
answerAll' pid = getQuestion pid >>= \case
  Just (QuestionLabel "$label.fortitudeTest" _ _ _) -> pure ()
  Just _ -> answerQuestion pid 0 >> answerAll' pid
  Nothing -> pure ()

-- | Setup: the box's Ring-bearer, the hand kept, stage 1A's setup, Brake of Thorns' damage
-- on Beravor and the first location offered added.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseRingBearer pid frodo
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "perform setup" pid (const True)
  x <- (`div` 10) <$> threatOf pid
  b <- toTarget <$> getHeroWithCode beravor
  replicateM_ x (chooseTarget pid b)
  answerAll pid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

-- | The player's threat set to 30: a Fortitude test discards 3 cards.
threatAt30 :: PlayerId -> GameT ()
threatAt30 pid = do
  t <- threatOf pid
  if t < 30 then run (RaiseThreat pid (30 - t)) else when (t > 30) (run $ LowerThreat pid (t - 30))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyPlacement :: EnemyId -> GameT Placement
enemyPlacement eid = getsGame (maybe Unplaced (.placement) . Map.lookup eid . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

-- | Commit the heroes to the open Fortitude test, discard the cards and pass the action
-- windows.
commitAndResolve :: PlayerId -> [CardCode] -> GameT ()
commitAndResolve pid codes = do
  for_ codes \c -> chooseTarget pid . toTarget =<< getHeroWithCode c
  chooseLabel "$label.doneCommittingToFortitudeTest" pid
  passActions pid

fortitude :: PlayerId -> [Message] -> GameT ()
fortitude pid onFail = run $ MakeFortitudeTest $ FortitudeTest pid GameSource [] 0 0 [] [] onFail []

offersTarget :: PlayerId -> Target -> GameT Bool
offersTarget pid t = maybe False (any \case TargetLabel t' _ -> t' == t; _ -> False) . fmap questionChoices <$> getQuestion pid

spec :: Spec
spec = describe "Mount Doom" do
  it "is a scenario the game can be dealt" . doomTest $ \_ ->
    isJust (lookupScenario "mount-doom") `shouldBe` True

  describe "setup" do
    it "builds the encounter deck from Mount Doom and Deep Shadows, its set-aside cards out of it" . doomTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.orcHold, Locations.mordorRoad, Locations.plainOfGorgoroth, Locations.brakeOfThorns, Locations.oldWaterCourse, Enemies.gollumMountDoom, Enemies.ringwraithOfSauron, Enemies.orcCaptain, Enemies.bigUruk, Enemies.littleSnufflerMountDoom, Treacheries.wickedMaster, Treacheries.theLidlessEye, Treacheries.foulFumes, Treacheries.heavyAndTired, Treacheries.drawnToTheRing, Treacheries.deepShadows, Treacheries.evilVigilance]
        `shouldBe` [3, 2, 4, 2, 1, 1, 2, 2, 3, 4, 1, 4, 2, 2, 3, 2, 3]
      length deck `shouldBe` 41
      sort . map toCardCode . scenarioSetAside <$> scenarioAttrs
        `shouldReturn` sort (map toCardCode [Locations.mountDoom, Objectives.theTowerOfBaradDurMountDoom, Locations.brakeOfThorns])

    it "stages Mount Doom and The Tower of Barad-dur, makes Brake of Thorns active, its damage dealt, and adds a location for the player" . doomTest $ \pid -> do
      run Setup
      chooseRingBearer pid frodo
      chooseLabel "$label.keepHand" pid
      chooseOptionMatching "perform setup" pid (const True)
      -- Brake of Thorns' Forced effect: the tens digit of the player's threat in damage
      x <- (`div` 10) <$> threatOf pid
      b <- toTarget <$> getHeroWithCode beravor
      replicateM_ x (chooseTarget pid b)
      getDamage b `shouldReturn` x
      answerAll pid
      locations <- getsGame (map (\l -> (toCardCode l, l.placement)) . toList . (.entities.locations))
      lookup (toCardCode Locations.brakeOfThorns) locations `shouldBe` Just ActiveLocation
      lookup (toCardCode Locations.mountDoom) locations `shouldBe` Just StagingArea
      length [() | (_, StagingArea) <- locations] `shouldBe` 2
      getsGame (map toCardCode . toList . (.entities.objectives)) >>= (`shouldSatisfy` elem (toCardCode Objectives.theTowerOfBaradDurMountDoom))
      (.questFlipped) . toAttrs <$> getActiveQuest `shouldReturn` True

  describe "Dire" do
    it "raises the elimination level to 99 and the Valour threshold to 80; stage 1B gets 4 quest points a player" . doomTest $ \pid -> do
      setUp pid
      getEliminationLevel pid `shouldReturn` 99
      -- "increased to 99": a modifier to the level, as The One Ring's -5, leaves it at 99
      b <- getHeroWithCode beravor
      _ <- putAttachmentOn pid Attachments.theOneRing (toTarget b)
      getEliminationLevel pid `shouldReturn` 99
      threatAt30 pid
      run $ RaiseThreat pid 20
      selectAny PlayerAtValourThreat `shouldReturn` False
      q <- getActiveQuest
      getModifiers (toTarget q) >>= (`shouldSatisfy` elem (QuestPointModifier 4))

    it "lets non-boon player cards reduce threat by 1 a round, boons by as much as they read" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      run $ RaiseThreat pid 30
      b <- getHeroWithCode beravor
      valor <- putAttachmentOn pid Attachments.valor (toTarget b)
      let valorSource = case valor of
            AttachmentTarget aid -> AttachmentSource aid
            t -> error $ "not an attachment: " <> show t
      runQueueT (lowerThreatBy (HeroSource $ toId b) pid 3) >> void mainLoop
      threatOf pid `shouldReturn` 59
      runQueueT (lowerThreatBy (HeroSource $ toId b) pid 3) >> void mainLoop
      threatOf pid `shouldReturn` 59
      runQueueT (lowerThreatBy valorSource pid 3) >> void mainLoop
      threatOf pid `shouldReturn` 56

  describe "Fortitude tests" do
    it "passes when the committed willpower beats the Eye icons, and fails on a tie" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      -- Orc-hold 1, Big Uruk 1, Plain of Gorgoroth 0: Frodo's 2 willpower ties
      _ <- setEncounterDeck ([Locations.orcHold, Enemies.bigUruk] <> replicate 4 Locations.plainOfGorgoroth)
      fortitude pid [RaiseThreat pid 9]
      commitAndResolve pid [frodo]
      threatOf pid `shouldReturn` 39
      failedFortitudeTestThisRound pid `shouldReturn` True
      (.heroExhausted) . toAttrs <$> getHeroWithCode frodo `shouldReturn` True
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Enemies.bigUruk))
      -- 3 Plains: Beravor's 2 willpower against none
      fortitude pid [RaiseThreat pid 9]
      commitAndResolve pid [beravor]
      threatOf pid `shouldReturn` 39

    it "adds an Eye icon for each Brake of Thorns in the staging area" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      _ <- stageLocation Locations.brakeOfThorns
      _ <- setEncounterDeck (replicate 3 Locations.plainOfGorgoroth)
      fortitude pid [RaiseThreat pid 9]
      commitAndResolve pid []
      threatOf pid `shouldReturn` 39

    it "has Gollum discarded for the test engage the first player and ignore a card's Eye icons" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      cards <- setEncounterDeck [Enemies.gollumMountDoom, Treacheries.wickedMaster, Locations.plainOfGorgoroth]
      fortitude pid [RaiseThreat pid 9]
      chooseTarget pid . toTarget =<< getHeroWithCode frodo
      chooseLabel "$label.doneCommittingToFortitudeTest" pid
      chooseTarget pid (CardIdTarget (fromJustNote "wicked" (atMay cards 1)).id)
      passActions pid
      gollum <- enemyOf Enemies.gollumMountDoom
      enemyPlacement gollum `shouldReturn` EngagedWith pid
      threatOf pid `shouldReturn` 30

    it "has Heavy and Tired go to the hero with the most attachments, keep it from the test, and exhaust it on a fail" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      -- Frodo, with The One Ring, has the most attachments
      f <- toTarget <$> getHeroWithCode frodo
      _ <- setEncounterDeck [Treacheries.heavyAndTired]
      run RevealEncounterCard
      answerAll pid
      tid <- getsGame (fromJustNote "heavy" . fmap toId . headMay . toList . (.entities.treacheries))
      getsGame (fmap (.placement) . Map.lookup tid . (.entities.treacheries)) `shouldReturn` Just (AttachedTo f)
      (.heroExhausted) . toAttrs <$> getHeroWithCode frodo `shouldReturn` True
      run $ Ready f
      _ <- setEncounterDeck (replicate 3 Treacheries.wickedMaster)
      fortitude pid []
      offersTarget pid f `shouldReturn` False
      commitAndResolve pid []
      answerAll pid
      (.heroExhausted) . toAttrs <$> getHeroWithCode frodo `shouldReturn` True
      -- itself a non-objective attachment, it is discarded; The One Ring stays
      getsGame (Map.member tid . (.entities.treacheries)) `shouldReturn` False
      getsGame (map toCardCode . filter ((== AttachedTo f) . (.placement)) . toList . (.entities.objectives)) >>= (`shouldSatisfy` elem (toCardCode Objectives.theOneRingTheMountainOfFire))

    it "has Ringwraith of Sauron in the staging area engage a player who fails" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      run . AddToStaging =<< genEncounterCard Enemies.ringwraithOfSauron
      wraith <- enemyOf Enemies.ringwraithOfSauron
      _ <- setEncounterDeck (replicate 6 Treacheries.wickedMaster)
      fortitude pid []
      commitAndResolve pid []
      answerAll pid
      enemyPlacement wraith `shouldReturn` EngagedWith pid

  describe "locations" do
    it "gives Mount Doom the first player's tens digit as threat and bars travel until 20 progress on stage 3B" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      doom <- locationOf Locations.mountDoom
      getLocationThreat doom `shouldReturn` 3
      getModifiers (LocationTarget doom) >>= (`shouldSatisfy` \ms -> CannotBeTraveledTo `elem` ms && ImmuneToPlayerCardEffects `elem` ms)

    it "gives Plain of Gorgoroth +2 threat once a player has failed a Fortitude test this round" . doomTest $ \pid -> do
      setUp pid
      plain <- stageLocation Locations.plainOfGorgoroth
      printed <- getLocationThreat plain
      threatAt30 pid
      _ <- setEncounterDeck (replicate 3 Treacheries.wickedMaster)
      fortitude pid []
      commitAndResolve pid []
      getLocationThreat plain `shouldReturn` (printed + 2)

  describe "stages" do
    it "has stage 2A travel to Mordor Road, and stage 2B advance at the end of the quest phase once there are as many resources as players" . doomTest $ \pid -> do
      setUp pid
      q <- putQuestStage Quests.theRoadToIsenmouthe
      run $ Flip q
      answerAll pid
      road <- locationOf Locations.mordorRoad
      getsGame (fmap (.placement) . Map.lookup road . (.entities.locations)) `shouldReturn` Just ActiveLocation
      run $ PlaceTokens GameSource q Resource 1
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      answerAll pid
      -- defeated: the next stage of the quest deck is in play
      (/= q) . toTarget <$> getActiveQuest `shouldReturn` True

    it "limits stage 3B to 5 progress a round once the first player fails its test, and keeps it until Mount Doom is active" . doomTest $ \pid -> do
      setUp pid
      q <- putQuestStage Quests.theLastGasp
      _ <- setEncounterDeck [Locations.orcHold]
      run $ Flip q
      answerAll pid
      threatAt30 pid
      _ <- setEncounterDeck (replicate 3 Treacheries.wickedMaster)
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      answerAll' pid
      commitAndResolve pid []
      run $ PlaceTokens GameSource q Progress 8
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 5
      run EndRound
      run $ PlaceTokens GameSource q Progress 20
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.theLastGasp

  describe "The Lidless Eye" do
    it "adds 2 Eye icons with The One Ring exhausted, raising the threat of a player who fails by 10" . doomTest $ \pid -> do
      setUp pid
      threatAt30 pid
      ring <- getsGame (fromJustNote "ring" . fmap toId . find ((== toCardCode Objectives.theOneRingTheMountainOfFire) . toCardCode) . toList . (.entities.objectives))
      run $ Exhaust (ObjectiveTarget ring)
      -- 3 Plains: Frodo's 2 willpower against the 2 added
      _ <- setEncounterDeck ([Treacheries.theLidlessEye] <> replicate 3 Locations.plainOfGorgoroth)
      run RevealEncounterCard
      answerAll' pid
      commitAndResolve pid [frodo]
      threatOf pid `shouldReturn` 40

  describe "The Tower of Barad-dur" do
    it "keeps threat from rising in the refresh phase and raises it for each non-Fellowship character at the end of the round" . doomTest $ \pid -> do
      setUp pid
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (RefreshThreatRaise 0))
      threat <- threatOf pid
      run $ CheckWindows [mkWhen RoundEnds]
      answerAll pid
      -- the test deck's three heroes; Frodo is of the Fellowship
      threatOf pid `shouldReturn` (threat + 3)

  describe "campaign mode" do
    let campaignLog = recordCount BlackGateResources "146024" 2 (newCampaignLog "the-lord-of-the-rings")
        campaignTest = campaignTestIn mountDoom campaignLog testDeck

    it "places the resources recorded for The Black Gate on the campaign card, the players losing once the last is removed" . campaignTest $ \pid -> do
      setUp pid
      (.cardResources) <$> getCampaignLog `shouldReturn` 2
      run $ CheckWindows [mkWhen RoundEnds]
      answerAll pid
      (.cardResources) <$> getCampaignLog `shouldReturn` 1
      run $ CheckWindows [mkWhen RoundEnds]
      answerAll pid
      getsGame (.outcome) `shouldReturn` Just Lost

    it "has the first player keep The One Ring as the Ring-bearer completes his quest, losing the campaign" . campaignTest $ \pid -> do
      setUp pid
      q <- putQuestStage Quests.theCrackOfDoom
      _ <- setEncounterDeck [Enemies.bigUruk]
      run $ Flip q
      answerAll pid
      doom <- locationOf Locations.mountDoom
      run $ PlaceLocation doom ActiveLocation
      answerAll pid
      _ <- setEncounterDeck (replicate 8 Locations.plainOfGorgoroth)
      run $ PlaceTokens GameSource (LocationTarget doom) Progress 9
      commitAndResolve pid [frodo]
      chooseLabel "$label.mountDoom.keep" pid
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      map (.completedScenario) l.completed `shouldBe` ["mount-doom"]
      nextScenario l `shouldBe` Nothing
      Map.member CampaignLost l.notes `shouldBe` True
