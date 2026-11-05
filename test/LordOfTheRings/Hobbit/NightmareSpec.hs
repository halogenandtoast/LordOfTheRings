module LordOfTheRings.Hobbit.NightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (weMustAwayEreBreakOfDay)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (..), modeL, sackDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport hiding (before, first, second)

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn (overAttrs (modeL .~ NightmareMode) weMustAwayEreBreakOfDay) testDeck

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

reveal :: CardDef -> GameT ()
reveal def = do
  _ <- setEncounterDeck [def]
  run RevealEncounterCard
  resolveRevealed

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (toId . fromJustNote "no enemy" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (toId . fromJustNote "no location" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

setSacks :: [CardDef] -> GameT ()
setSacks defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (sackDeckL .~ cards)

sackCodes :: GameT [CardCode]
sackCodes = getsGame (map toCardCode . (.sackDeck) . toAttrs . (.scenario))

spec :: Spec
spec = describe "Hobbit Nightmare" do
  describe "We Must Away, Ere Break of Day" do
    it "removes the specified copies, gathers the Nightmare cards, and keeps sacks and Trolls separate" . nightmare $ \_ -> do
      run $ DoStep 1 Setup
      attrs <- getsGame (toAttrs . (.scenario))
      map (`copies` attrs.encounterDeck) [Enemies.aSuspiciousCrow, Locations.loneLands, Locations.drearyHills, Locations.hobbitLands, Treacheries.aNicePickle]
        `shouldBe` [0, 1, 1, 0, 0]
      map (`copies` attrs.encounterDeck) [Enemies.aMurderOfCrows, Locations.redRiverBank, Locations.wickedHilltops, Locations.comfortableLight, Treacheries.minceThemFine, Treacheries.nightWanesSlow, Treacheries.oneByOne]
        `shouldBe` [3, 3, 3, 3, 3, 2, 2]
      length attrs.sackDeck `shouldBe` 7
      sort (map toCardCode attrs.scenarioSetAside) `shouldBe` sort (map toCardCode [Enemies.william, Enemies.bert, Enemies.tom, Locations.trollCave])
      copies Nightmare.weMustAwayEreBreakOfDayNightmare attrs.encounterDeck `shouldBe` 0

    it "keeps the normal encounter deck free of Nightmare cards" . gameTestIn weMustAwayEreBreakOfDay testDeck $ \_ -> do
      run $ DoStep 1 Setup
      cards <- getEncounterDeck
      any ((== Just EncounterSet.WeMustAwayEreBreakOfDayNightmare) . cdEncounterSet . toCardDef) cards `shouldBe` False
      copies Enemies.aSuspiciousCrow cards `shouldBe` 3

    it "gives Troll Key and Troll Purse surge when revealed" . nightmare $ \_ -> do
      _ <- setEncounterDeck [Objectives.trollKey, Objectives.trollPurse, Locations.oldForestRoad]
      run RevealEncounterCard
      getsGame (sort . map toCardCode . toList . (.entities.objectives)) `shouldReturn` sort (map toCardCode [Objectives.trollKey, Objectives.trollPurse])
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.oldForestRoad]
      getEncounterDeck `shouldReturn` []

    it "has the Crows reveal from the discard pile, then replaces destruction with shuffling" . nightmare $ \_ -> do
      old <- genEncounterCard Locations.oldForestRoad
      run $ AddToEncounterDiscard [old]
      reveal Enemies.aMurderOfCrows
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.oldForestRoad]
      eid <- getsGame (toId . fromJustNote "no Crows" . headMay . toList . (.entities.enemies))
      run $ Destroy GameSource (EnemyTarget eid)
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      copies Enemies.aMurderOfCrows <$> getEncounterDeck `shouldReturn` 1
      copies Enemies.aMurderOfCrows <$> getEncounterDiscard `shouldReturn` 0

    it "discards the Crows normally when they are discarded rather than destroyed" . nightmare $ \_ -> do
      eid <- stageEnemy Enemies.aMurderOfCrows
      run $ Discard GameSource (EnemyTarget eid)
      copies Enemies.aMurderOfCrows <$> getEncounterDiscard `shouldReturn` 1
      copies Enemies.aMurderOfCrows <$> getEncounterDeck `shouldReturn` 0

    it "moves only the top discarded card to the bottom and raises threat by its printed threat" . nightmare $ \pid -> do
      river <- stageLocation Locations.redRiverBank
      cards <- traverse genEncounterCard [Enemies.aMurderOfCrows, Locations.oldForestRoad]
      run $ AddToEncounterDiscard cards
      _ <- setEncounterDeck [Locations.forestGate]
      before <- (.threat) <$> getPlayer pid
      run $ CheckWindows [mkAfter $ PhaseBegins RefreshPhase]
      (.threat) <$> getPlayer pid `shouldReturn` (before + 4)
      map toCardCode <$> getEncounterDeck `shouldReturn` map toCardCode [Locations.forestGate, Enemies.aMurderOfCrows]
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.oldForestRoad]
      (.progress) <$> getLocation river `shouldReturn` 0

    it "makes each player discard an unsacked ally after progress, leaving a sacked ally alone" . nightmare $ \pid -> do
      hill <- stageLocation Locations.wickedHilltops
      first <- putCardIntoHand pid Allies.snowbournScout
      second <- putCardIntoHand pid Allies.guardOfTheCitadel
      run $ PutAllyIntoPlay pid first.id
      run $ PutAllyIntoPlay pid second.id
      allies <- getsGame (toList . (.entities.allies))
      let scout = toTarget $ fromJustNote "no scout" $ find ((== toCardCode Allies.snowbournScout) . toCardCode) allies
      run . (`PutObjectiveIntoPlayAttached` scout) =<< genEncounterCard Objectives.aSmellySack
      run $ PlaceTokens GameSource (LocationTarget hill) Progress 1
      getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.snowbournScout]

    it "gives Wicked Hilltops' shadow +3 attack to a Troll, +2 to another enemy" . nightmare $ \pid -> do
      troll <- stageEnemy Enemies.bert
      crow <- stageEnemy Enemies.aMurderOfCrows
      trollBefore <- getEnemyAttack troll
      crowBefore <- getEnemyAttack crow
      shadow <- genEncounterCard Locations.wickedHilltops
      run $ AttachShadowCard troll shadow
      run $ ResolveShadow troll pid []
      getEnemyAttack troll `shouldReturn` (trollBefore + 3)
      otherShadow <- genEncounterCard Locations.wickedHilltops
      run $ AttachShadowCard crow otherShadow
      run $ ResolveShadow crow pid []
      getEnemyAttack crow `shouldReturn` (crowBefore + 2)

    it "prevents progress on Comfortable Light and sacks a character before discarding the traveled location" . nightmare $ \_ -> do
      light <- stageLocation Locations.comfortableLight
      setSacks [Objectives.aDeepSack]
      getModifiers light >>= (`shouldSatisfy` elem (ProgressLimit 0))
      run $ PlaceTokens GameSource (LocationTarget light) Progress 20
      (.progress) <$> getLocation light `shouldReturn` 0
      run $ TravelTo light
      getsGame (length . (.entities.locations)) `shouldReturn` 0
      hero <- getHeroWithCode "01007"
      getsGame (map (.placement) . toList . (.entities.objectives)) `shouldReturn` [AttachedTo $ toTarget hero]

    it "shuffles the discard pile and reveals an additional card for Night Wanes Slow" . nightmare $ \_ -> do
      old <- genEncounterCard Locations.oldForestRoad
      run $ AddToEncounterDiscard [old]
      reveal Treacheries.nightWanesSlow
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.oldForestRoad]
      copies Treacheries.nightWanesSlow <$> getEncounterDiscard `shouldReturn` 1

    it "returns the destroyed character's Sack to the top and sacks the next eligible character" . nightmare $ \pid -> do
      hero <- getHeroWithCode "01007"
      run . (`PutObjectiveIntoPlayAttached` toTarget hero) =<< genEncounterCard Objectives.aDeepSack
      setSacks [Objectives.aSmellySack, Objectives.aWornSack]
      reveal Treacheries.oneByOne
      -- Deep Sack now chooses Beravor (2 willpower), rather than drawing Smelly Sack.
      getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` notElem "01007")
      getsGame (map toCardCode . toList . (.entities.objectives)) `shouldReturn` [toCardCode Objectives.aDeepSack]
      sackCodes `shouldReturn` map toCardCode [Objectives.aSmellySack, Objectives.aWornSack]
      getQuestion pid `shouldReturn` Nothing

    it "does not sack anyone for One by One when no character has a Sack" . nightmare $ \_ -> do
      setSacks [Objectives.aDeepSack]
      reveal Treacheries.oneByOne
      sackCodes `shouldReturn` [toCardCode Objectives.aDeepSack]

    it "interrupts stage 2 progress at Night Wanes Slow, leaving the rest of the deck undiscarded" . nightmare $ \pid -> do
      run $ DoStep 1 Setup
      initial <- getActiveQuest
      run $ LeaveQuestStage initial.id
      card <- genEncounterCard Quests.roastMutton
      run $ AdvanceToQuest card
      resolveRevealed
      _ <- setEncounterDeck [Locations.oldForestRoad, Treacheries.nightWanesSlow, Locations.forestGate, Locations.wickedHilltops]
      putGame . (phaseL .~ QuestPhase) =<< getGame
      q <- getActiveQuest
      hero <- getHeroWithCode "01007"
      before <- getHeroResources hero.id
      pushAll [PlaceTokens GameSource (toTarget q) Progress 4, PlaceTokens GameSource (toTarget hero) Resource 10, EndPhase QuestPhase]
      void mainLoop
      map toCardCode <$> getEncounterDeck `shouldReturn` map toCardCode [Locations.forestGate, Locations.wickedHilltops]
      getHeroResources hero.id `shouldReturn` before
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.roastMutton
      -- The test stops at the next phase's action window.
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "does not interrupt the phase for a non-quest discard of Night Wanes Slow" . nightmare $ \_ -> do
      _ <- setEncounterDeck [Treacheries.nightWanesSlow, Locations.forestGate]
      putGame . (phaseL .~ QuestPhase) =<< getGame
      hero <- getHeroWithCode "01007"
      before <- getHeroResources hero.id
      pushAll [DiscardTopOfEncounterDeck GameSource GameTarget 1, PlaceTokens GameSource (toTarget hero) Resource 1]
      void mainLoop
      getHeroResources hero.id `shouldReturn` (before + 1)

    it "advances from stage 1 after Mince Them Fine even when no Troll is engaged" . nightmare $ \_ -> do
      run $ DoStep 1 Setup
      _ <- setEncounterDeck [Treacheries.minceThemFine, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.roastMutton

    it "makes an engaged Troll attack immediately for Mince Them Fine" . nightmare $ \pid -> do
      troll <- stageEnemy Enemies.bert
      run $ EngageEnemy pid troll
      defender <- getHeroWithCode "01012"
      reveal Treacheries.minceThemFine
      passActions pid
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.declareDefender" `isPrefixOf` l; _ -> False)
      chooseTarget pid defender
      passActions pid
      getDamage (toTarget defender) `shouldReturn` 3

    it "returns a Troll to staging after the attack for Mince Them Fine's shadow" . nightmare $ \pid -> do
      troll <- stageEnemy Enemies.bert
      run $ EngageEnemy pid troll
      defender <- getHeroWithCode "01012"
      _ <- setEncounterDeck [Treacheries.minceThemFine]
      run $ DealShadowCard troll
      run $ EnemyAttack pid troll
      passActions pid
      chooseTarget pid defender
      passActions pid
      getDamage (toTarget defender) `shouldReturn` 3
      (.placement) <$> getEnemy troll `shouldReturn` StagingArea

    it "leaves a non-Troll engaged after Mince Them Fine's shadow" . nightmare $ \pid -> do
      crow <- stageEnemy Enemies.aMurderOfCrows
      run $ EngageEnemy pid crow
      defender <- getHeroWithCode "01012"
      _ <- setEncounterDeck [Treacheries.minceThemFine]
      run $ DealShadowCard crow
      run $ EnemyAttack pid crow
      passActions pid
      chooseTarget pid defender
      passActions pid
      (.placement) <$> getEnemy crow `shouldReturn` EngagedWith pid
