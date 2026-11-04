module LordOfTheRings.TheLordOfTheRings.AShadowOfThePastSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (..), newCampaignLog)
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithId), theOneRingExhausted)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (aShadowOfThePast, lookupScenario)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (..), mkAfter)
import TestImport

shadowTest :: (PlayerId -> GameT ()) -> IO ()
shadowTest = gameTestIn aShadowOfThePast testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

campaignTest :: (PlayerId -> GameT ()) -> IO ()
campaignTest = campaignTestIn aShadowOfThePast lotr testDeck

-- | Setup, The Black Riders' Frodo Baggins taken should the game offer several
-- Ring-bearers, the starting hand kept and stage 1A clicked.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  getQuestion pid >>= \case
    Just (QuestionLabel l _ _ _) | "$label.saga.chooseFellowshipHero" `isPrefixOf` l -> void $ answerQuestion pid 0
    _ -> pure ()
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

frodo :: GameT Target
frodo = hero (toCardCode Heroes.frodoBagginsTheBlackRiders)

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

enemiesEngagedWith :: PlayerId -> GameT Int
enemiesEngagedWith pid = getsGame (length . filter ((== EngagedWith pid) . (.placement)) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationAttrs :: LocationId -> GameT LocationAttrs
locationAttrs lid = getsGame (toAttrs . fromJustNote "no such location" . lookup lid . (.entities.locations))

treacheriesOn :: Target -> GameT [CardCode]
treacheriesOn host = getsGame (map toCardCode . filter ((== AttachedTo host) . (.placement)) . toList . (.entities.treacheries))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

-- | Reveal the first of the cards, the rest beneath it in the encounter deck.
reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The Hide test asked: commit the characters, then take no action in its window.
hide :: PlayerId -> [Target] -> GameT ()
hide pid cs = do
  for_ cs (chooseTarget pid)
  chooseLabel "$label.doneCommittingToHide" pid
  passActions pid

-- | Make a Hide test against the cards on top of the encounter deck.
hideAgainst :: PlayerId -> Int -> [CardDef] -> [Target] -> GameT ()
hideAgainst pid n defs cs = do
  _ <- setEncounterDeck defs
  run . MakeHideTest $ HideTest pid GameSource n [] []
  hide pid cs

failedThisRound :: PlayerId -> GameT Bool
failedThisRound pid = elem FailedHideTestThisRound <$> getModifiers (PlayerTarget pid)

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

spec :: Spec
spec = describe "A Shadow of the Past" do
  it "is a scenario the game can be dealt" . shadowTest $ \_ ->
    isJust (lookupScenario "a-shadow-of-the-past") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "stages a Black Rider, makes Bag End active, sets Bucklebury Ferry aside and gives the first player Frodo with The One Ring" . shadowTest $ \pid -> do
      setUp pid
      rider <- enemyOf Enemies.blackRider
      placementOfEnemy rider `shouldReturn` StagingArea
      bagEnd <- locationAttrs =<< locationOf Locations.bagEnd
      bagEnd.placement `shouldBe` ActiveLocation
      attrs <- scenarioAttrs
      map toCardCode attrs.scenarioSetAside `shouldBe` [toCardCode Locations.buckleburyFerry]
      all ((`elem` map Just [AShadowOfThePast, Hunted, TheRing, TheBlackRiders]) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      copies Treacheries.theRingDrawsThem attrs.encounterDeck `shouldBe` 0
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.aShortcutToMushrooms, Quests.escapeToBuckland]
      activeQuestCode `shouldReturn` toCardCode Quests.threeIsCompany
      f <- frodo
      ring <- getsGame (find ((== toCardCode Objectives.theOneRingTheBlackRiders) . toCardCode) . toList . (.entities.objectives))
      fmap (.placement) ring `shouldBe` Just (AttachedTo f)
      length . (.hand) <$> getPlayer pid `shouldReturn` 6

    it "builds the encounter deck in normal and easy mode" . shadowTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      let counted = [Enemies.blackRider, Enemies.evilCrowAShadowOfThePast, Locations.stockRoad, Locations.theMarish, Treacheries.crawlingTowardsHim, Treacheries.piercingCry, Treacheries.blackSteed, Treacheries.lureOfTheRing, Treacheries.rodeLikeAGale]
      map (`copies` deck) counted `shouldBe` [5, 3, 3, 2, 3, 3, 3, 3, 2]
      -- Bucklebury Ferry is set aside
      length deck `shouldBe` 42

    it "keeps fewer copies in easy mode" . gameTestIn (overAttrs (modeL .~ EasyMode) aShadowOfThePast) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.blackRider, Locations.stockRoad, Locations.theMarish, Treacheries.crawlingTowardsHim, Treacheries.piercingCry, Treacheries.blackSteed]
        `shouldBe` [3, 1, 1, 2, 1, 1]

    it "in campaign mode attaches Mr. Underhill to Frodo, stages Gandalf's Delay and shuffles in Gildor Inglorion and The Ring Draws Them" . campaignTest $ \pid -> do
      setUp pid
      f <- frodo
      underhill <- getsGame (find ((== toCardCode Attachments.mrUnderhill) . toCardCode) . toList . (.entities.attachments))
      fmap (.placement) underhill `shouldBe` Just (AttachedTo f)
      delay <- getsGame (find ((== toCardCode Objectives.gandalfsDelay) . toCardCode) . toList . (.entities.objectives))
      fmap (.placement) delay `shouldBe` Just StagingArea
      deck <- getEncounterDeck
      map (`copies` deck) [Objectives.gildorInglorionObjective, Treacheries.theRingDrawsThem] `shouldBe` [1, 1]
      -- Gandalf's Delay: each player draws 1 less card in his starting hand
      length . (.hand) <$> getPlayer pid `shouldReturn` 5

  -- \* Hide tests

  describe "Hide tests" do
    it "has the player who reveals a Black Rider make a Hide 2 test, passing on a tie" . shadowTest $ \pid -> do
      setUp pid
      eowyn <- hero "01007"
      -- Éowyn's 4 willpower against two Stock Roads' 6 threat fails; against a Stock Road
      -- and a treachery's none it passes
      reveal [Enemies.blackRider, Locations.stockRoad, Treacheries.lureOfTheRing]
      hide pid [eowyn]
      failedThisRound pid `shouldReturn` False
      discard <- getEncounterDiscard
      sort (map toCardCode discard) `shouldBe` sort (map toCardCode [Locations.stockRoad, Treacheries.lureOfTheRing])

    it "fails a test the discarded cards' threat beats, each Nazgûl in the staging area engaging the player, then each Black Rider engaged attacks" . shadowTest $ \pid -> do
      setUp pid
      reveal [Enemies.blackRider, Locations.stockRoad, Locations.stockRoad]
      hide pid []
      failedThisRound pid `shouldReturn` True
      enemiesEngagedWith pid `shouldReturn` 2
      rider <- enemyOf Enemies.blackRider
      offersAbilityOf pid (EnemyTarget rider) `shouldReturn` True

    it "counts Woody End lowering the Hide value, Evil Crow raising it, Stock Road's threat and Green Hill Country's willpower" . shadowTest $ \pid -> do
      theodred <- hero "01002"
      -- Woody End active: a Hide 2 test discards 1 card
      woody <- stageLocation Locations.woodyEnd
      run $ PlaceLocation woody ActiveLocation
      hideAgainst pid 2 [Locations.greenHillCountry, Locations.greenHillCountry] [theodred]
      length <$> getEncounterDiscard `shouldReturn` 1
      failedThisRound pid `shouldReturn` True
      -- Evil Crow engaged: back to 2 cards; Green Hill Country active gives Théodred +1
      _ <- engaged pid Enemies.evilCrowAShadowOfThePast
      run $ Ready theodred
      ghc <- stageLocation Locations.greenHillCountry
      run $ PlaceLocation woody StagingArea
      run $ PlaceLocation ghc ActiveLocation
      discarded0 <- length <$> getEncounterDiscard
      hideAgainst pid 2 [Treacheries.lureOfTheRing, Locations.woodyEnd] [theodred]
      subtract discarded0 . length <$> getEncounterDiscard `shouldReturn` 2
      -- Stock Road in the staging area adds 2: Woody End's 1 + 2 against Théodred's 2
      _ <- stageLocation Locations.stockRoad
      run $ Ready theodred
      p <- (.threat) <$> getPlayer pid
      hideAgainst pid 2 [Treacheries.lureOfTheRing, Treacheries.lureOfTheRing] [theodred]
      threatOf pid `shouldReturn` p

  -- \* The stages

  describe "Three is Company" do
    it "cannot be defeated until Bag End is in the victory display" . shadowTest $ \pid -> do
      setUp pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 5
      activeQuestCode `shouldReturn` toCardCode Quests.threeIsCompany
      bagEnd <- locationOf Locations.bagEnd
      run $ PlaceTokens GameSource (LocationTarget bagEnd) Progress 3
      useForcedAbilities pid
      getQuestion pid >>= \case
        Just _ -> chooseOptionMatching "pass Bag End's response" pid (const True)
        Nothing -> pure ()
      resolveRevealed
      activeQuestCode `shouldReturn` toCardCode Quests.aShortcutToMushrooms

  describe "A Shortcut to Mushrooms" do
    it "reveals a Black Rider from the discard pile and adds 1 threat to a Hide test for each ally" . shadowTest $ \pid -> do
      -- the ally adds 1 to the Black Rider's Hide 2 test against a treachery's no threat
      _ <- putAllyIntoPlay pid Allies.gondorianSpearman
      _ <- setEncounterDeck [Treacheries.lureOfTheRing]
      run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.blackRider
      _ <- putQuestStage Quests.aShortcutToMushrooms
      q <- getActiveQuest
      run $ Flip (toTarget q)
      resolveWhenRevealed pid
      chooseOptionMatching "take the Black Rider" pid \case TargetLabel {} -> True; _ -> False
      hide pid []
      rider <- enemyOf Enemies.blackRider
      placementOfEnemy rider `shouldReturn` EngagedWith pid

  describe "Escape to Buckland" do
    it "adds Bucklebury Ferry, and places progress on a non-unique location after a successful Hide test" . shadowTest $ \pid -> do
      ferry <- genEncounterCard Locations.buckleburyFerry
      putGame . (scenarioL %~ overAttrs (setAsideL .~ [ferry])) =<< getGame
      _ <- setEncounterDeck []
      _ <- putQuestStage Quests.escapeToBuckland
      q <- getActiveQuest
      run $ Flip (toTarget q)
      resolveWhenRevealed pid
      -- no Black Rider to find
      chooseOptionMatching "find nothing" pid (const True)
      _ <- locationOf Locations.buckleburyFerry
      road <- stageLocation Locations.stockRoad
      run $ PlaceLocation road ActiveLocation
      hideAgainst pid 1 [Treacheries.lureOfTheRing] []
      failedThisRound pid `shouldReturn` False
      -- Stock Road is the one non-unique location: placed without asking
      chooseAbilityOf pid (QuestTarget $ toId q)
      (.progress) <$> locationAttrs road `shouldReturn` 2

  -- \* Locations

  describe "Bucklebury Ferry" do
    it "cannot be traveled to while another location is in play, has the first player make a Hide 3 test to travel, and wins the game as it is explored" . shadowTest $ \pid -> do
      ferry <- stageLocation Locations.buckleburyFerry
      road <- stageLocation Locations.stockRoad
      getModifiers (LocationTarget ferry) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ Discard (LocationTarget road)
      getModifiers (LocationTarget ferry) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      _ <- setEncounterDeck [Treacheries.lureOfTheRing, Treacheries.lureOfTheRing, Treacheries.lureOfTheRing]
      run $ TravelTo ferry
      hide pid []
      (.placement) <$> locationAttrs ferry `shouldReturn` ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget ferry) Progress 9
      getsGame (.outcome) `shouldReturn` Just Won

    it "has the players choose the campaign card's burden and boon as they win, Mr. Underhill recorded on the Ring-bearer" . campaignTest $ \pid -> do
      setUp pid
      ferry <- stageLocation Locations.buckleburyFerry
      run $ PlaceLocation ferry ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget ferry) Progress 9
      chooseLabel "$label.aShadowOfThePast.theRingDrawsThem" pid
      chooseLabel "$label.aShadowOfThePast.mrUnderhill" pid
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      [(p.code, p.attachedTo) | p <- l.pool]
        `shouldBe` [(toCardCode Treacheries.theRingDrawsThem, Nothing), (toCardCode Attachments.mrUnderhill, Just $ toCardCode Heroes.frodoBagginsTheBlackRiders)]

  describe "Stock-brook" do
    it "is traveled to as it is revealed, the active location returned to the staging area" . shadowTest $ \pid -> do
      setUp pid
      bagEnd <- locationOf Locations.bagEnd
      reveal [Locations.stockBrook]
      brook <- locationOf Locations.stockBrook
      (.placement) <$> locationAttrs brook `shouldReturn` ActiveLocation
      (.placement) <$> locationAttrs bagEnd `shouldReturn` StagingArea

  describe "The Marish" do
    it "shuffles a Nazgûl from the discard pile into the encounter deck after a failed Hide test" . shadowTest $ \pid -> do
      _ <- stageLocation Locations.theMarish
      run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.blackRider
      hideAgainst pid 1 [Locations.stockRoad] []
      useForcedAbilities pid
      deck <- getEncounterDeck
      copies Enemies.blackRider deck `shouldBe` 1

  -- \* Treacheries

  describe "Crawling Towards Him" do
    it "removes the revealer's characters from the quest when he failed its Hide test" . shadowTest $ \pid -> do
      eowyn <- hero "01007"
      run $ CommitToQuest eowyn
      inPhase QuestPhase
      reveal [Treacheries.crawlingTowardsHim, Locations.stockRoad, Locations.stockRoad]
      hide pid []
      resolveRevealed
      committed <- getsGame (heroCommitted . toAttrs . fromJustNote "no hero" . find ((== "01007") . toCardCode) . toList . (.entities.heroes))
      committed `shouldBe` False

  describe "Have You Seen Baggins?" do
    it "attaches to a hero and discards it after a failed Hide test" . shadowTest $ \pid -> do
      theodred <- hero "01002"
      reveal [Treacheries.haveYouSeenBaggins, Treacheries.lureOfTheRing]
      chooseTarget pid theodred
      resolveRevealed
      treacheriesOn theodred `shouldReturn` [toCardCode Treacheries.haveYouSeenBaggins]
      hideAgainst pid 1 [Locations.stockRoad] []
      useForcedAbilities pid
      getsGame (any ((== "01002") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` False

  describe "Hunting for the Ring" do
    it "has each player return an engaged Nazgûl or make a Hide 2 test" . shadowTest $ \pid -> do
      rider <- engaged pid Enemies.blackRider
      reveal [Treacheries.huntingForTheRing]
      chooseTarget pid (EnemyTarget rider)
      placementOfEnemy rider `shouldReturn` StagingArea

  describe "Pale Blade and Black Steed" do
    it "attach to a Nazgûl: +1 attack, no non-combat damage; -10 engagement cost and back to the staging area as the refresh phase ends" . shadowTest $ \pid -> do
      rider <- stage Enemies.blackRider
      reveal [Treacheries.paleBlade, Treacheries.blackSteed]
      resolveRevealed
      sort <$> treacheriesOn (EnemyTarget rider) `shouldReturn` sort (map toCardCode [Treacheries.paleBlade, Treacheries.blackSteed])
      getModifiers (EnemyTarget rider) >>= (`shouldSatisfy` elem CannotTakeNonCombatDamage)
      getEngagementCost rider `shouldReturn` 25
      run $ EngageEnemy pid rider
      run $ CheckWindows [Window When (PhaseEnds RefreshPhase)]
      useForcedAbilities pid
      placementOfEnemy rider `shouldReturn` StagingArea

  describe "Rode Like a Gale" do
    it "returns each engaged Nazgûl to the staging area" . shadowTest $ \pid -> do
      rider <- engaged pid Enemies.blackRider
      reveal [Treacheries.rodeLikeAGale]
      placementOfEnemy rider `shouldReturn` StagingArea

  describe "Lure of the Ring" do
    it "has the first player exhaust The One Ring or raise his threat by 3" . shadowTest $ \pid -> do
      setUp pid
      reveal [Treacheries.lureOfTheRing, Treacheries.lureOfTheRing, Treacheries.rodeLikeAGale]
      chooseLabel "$label.lureOfTheRing.exhaust" pid
      theOneRingExhausted `shouldReturn` True
      p <- threatOf pid
      -- surge: the second copy, the Ring already exhausted, raises threat
      resolveRevealed
      threatOf pid `shouldReturn` (p + 3)

  describe "The Ring Draws Them" do
    it "has each enemy in the staging area engage the first player at the end of the phase" . shadowTest $ \pid -> do
      rider <- stage Enemies.blackRider
      inPhase QuestPhase
      reveal [Treacheries.theRingDrawsThem, Treacheries.powerInTheirTerror]
      placementOfEnemy rider `shouldReturn` StagingArea
      -- the phase ends, as far as the engagement it brings
      runOnly $ EndPhase QuestPhase
      let untilEngaged :: Int -> GameT ()
          untilEngaged n = do
            p <- placementOfEnemy rider
            when (n > 0 && p == StagingArea) $ runNext >> untilEngaged (n - 1)
      untilEngaged 30
      placementOfEnemy rider `shouldReturn` EngagedWith pid

  describe "Power in their Terror" do
    it "deals 2 damage to each ally with 0 willpower once each ally gets -1" . shadowTest $ \pid -> do
      -- Gondorian Spearman, 0 willpower and 1 hit point, is destroyed
      _ <- putAllyIntoPlay pid Allies.gondorianSpearman
      reveal [Treacheries.powerInTheirTerror]
      getsGame (null . (.entities.allies)) `shouldReturn` True

  describe "Piercing Cry" do
    it "puts a Nazgûl from the encounter deck into play engaged with the first player" . shadowTest $ \pid -> do
      _ <- setEncounterDeck [Treacheries.piercingCry, Enemies.blackRider]
      run RevealEncounterCard
      resolveRevealed
      -- the one Black Rider is taken without asking
      chooseLabel "$label.piercingCry.engaged" pid
      rider <- enemyOf Enemies.blackRider
      placementOfEnemy rider `shouldReturn` EngagedWith pid

  -- \* Enemies

  describe "The Witch-king" do
    it "is immune, cannot be defended by characters with 1 willpower or less, and gets -30 engagement cost while The One Ring is exhausted" . shadowTest $ \pid -> do
      setUp pid
      wk <- stage Enemies.theWitchKing
      getModifiers (EnemyTarget wk) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      theodred <- hero "01002"
      getModifiers theodred >>= (`shouldSatisfy` elem (CannotDefendAgainst (EnemyWithId wk)))
      getEngagementCost wk `shouldReturn` 45
      ring <- getsGame (fmap toId . find ((== toCardCode Objectives.theOneRingTheBlackRiders) . toCardCode) . toList . (.entities.objectives))
      for_ ring (run . Exhaust . ObjectiveTarget)
      getEngagementCost wk `shouldReturn` 15

  describe "Gandalf's Delay and Gildor Inglorion" do
    it "discards Gildor Inglorion at the end of the round unless a Fellowship resource is spent" . campaignTest $ \pid -> do
      setUp pid
      reveal [Objectives.gildorInglorionObjective]
      gildor <- getsGame (fmap toId . find ((== toCardCode Objectives.gildorInglorionObjective) . toCardCode) . toList . (.entities.allies))
      isJust gildor `shouldBe` True
      f <- frodo
      run $ PlaceTokens GameSource f Resource 1
      run $ CheckWindows [mkAfter RoundEnds]
      useForcedAbilities pid
      chooseLabel "$label.gildorInglorionObjective.cancel" pid
      isJust <$> getQuestion pid `shouldReturn` False
      map toCardCode <$> getEncounterDiscard `shouldReturn` []
      getsGame (any ((== toCardCode Objectives.gildorInglorionObjective) . toCardCode) . toList . (.entities.allies)) `shouldReturn` True
