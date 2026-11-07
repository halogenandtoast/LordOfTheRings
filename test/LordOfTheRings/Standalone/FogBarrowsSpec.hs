module LordOfTheRings.Standalone.FogBarrowsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getCurrentQuest, getThreat, getHero)
import LordOfTheRings.Modifier (ModifierType (CannotBeDefeated, CannotBeTraveledTo, ProgressLimit, WillpowerModifier))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase, TravelPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (fogOnTheBarrowDowns)
import LordOfTheRings.Scenario.Types (scenarioSetAside, questDeckL)
import TestImport

progressOn :: Target -> GameT Int
progressOn target = getsGame (maybe 0 (.progress) . find ((== target) . toTarget) . toList . (.entities.quests))

stageOf :: CardDef -> GameT Target
stageOf def = getsGame (toTarget . fromJustNote "missing stage" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (toId . fromJustNote "missing location" . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

asideFours :: GameT [CardId]
asideFours = getsGame (map (.id) . filter ((== toCardCode Quests.trappedInsideABarrow) . toCardCode) . scenarioSetAside . toAttrs . (.scenario))

-- Keep physical quest cards and the full set-aside pool; control only encounter draws.
homeStage :: GameT Target
homeStage = do
  run $ DoStep 1 Setup
  initial <- getActiveQuest
  run $ PerformSetup (toTarget initial)
  run $ LeaveQuestStage (toId initial)
  card <- getsGame (fromJustNote "missing home stage" . find ((== toCardCode Quests.lostInTheFog) . toCardCode) . (.questDeck) . toAttrs . (.scenario))
  run $ PutQuestIntoPlay card
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (questDeckL .~ [])
  target <- stageOf Quests.lostInTheFog
  _ <- setEncounterDeck $ replicate 5 Locations.oldForestRoad
  run $ Flip target
  resolveRevealed
  pure target

enterBarrow :: PlayerId -> GameT ()
enterBarrow pid = do
  _ <- setEncounterDeck [Locations.greatBarrow, Enemies.coldWight, Locations.oldForestRoad, Locations.oldForestRoad]
  pushAll [SetRevealer (Just pid), RevealEncounterCard, SetRevealer Nothing]
  void mainLoop
  resolveRevealed

finishBarrowChoice :: PlayerId -> GameT ()
finishBarrowChoice pid = getQuestion pid >>= \case
  Just (QuestionLabel label _ _ _) | label == "$label.fog.removeGreatBarrow" ->
    chooseOptionMatching "retire a Great Barrow" pid (\case TargetLabel _ _ -> True; _ -> False)
  _ -> pure ()

returnFromBarrow :: PlayerId -> GameT ()
returnFromBarrow pid = do
  run $ FocusArea pid
  target <- stageOf Quests.trappedInsideABarrow
  run $ PlaceTokens GameSource target Progress 9
  getCurrentQuest `shouldReturn` Just target
  run JoinAreas
  finishBarrowChoice pid

spec :: Spec
spec = describe "Fog barrow sequence" do
  it "sets up the full physical encounter deck, three main stages, and four reusable private stages" . gameTestIn fogOnTheBarrowDowns testDeck $ \_ -> do
    run $ DoStep 1 Setup
    length <$> getEncounterDeck `shouldReturn` 41
    length <$> asideFours `shouldReturn` 4
    current <- getActiveQuest
    toCardCode current `shouldBe` toCardCode Quests.theHouseOfTomBombadil
    run $ PerformSetup (toTarget current)
    length <$> getEncounterDeck `shouldReturn` 34
    getsGame (length . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 11
    getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario)) `shouldReturn` map toCardCode [Quests.acrossTheDowns, Quests.lostInTheFog]

  it "adds Standing Stones and shuffles all five physical Great Barrows into stage 3's deck" . gameTestIn fogOnTheBarrowDowns testDeck $ \_ -> do
    home <- homeStage
    getCurrentQuest `shouldReturn` Just home
    void $ locationOf Locations.standingStones
    length . filter ((== toCardCode Locations.greatBarrow) . toCardCode) <$> getEncounterDeck `shouldReturn` 5
    getsGame (length . filter ((== toCardCode Locations.greatBarrow) . toCardCode) . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 0
    getModifiers home >>= (`shouldSatisfy` elem CannotBeDefeated)

  it "splits a solo player, finds a Wight without revealing it, returns at travel, and wins at the original stage" . gameTestIn fogOnTheBarrowDowns testDeck $ \pid -> do
    home <- homeStage
    pool <- asideFours
    enterBarrow pid
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.trappedInsideABarrow
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.coldWight]
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [StagingArea]
    length <$> asideFours `shouldReturn` 3
    returnFromBarrow pid
    getCurrentQuest `shouldReturn` Just home
    sort <$> asideFours `shouldReturn` sort pool
    getsGame (isNothing . gameSeparation) `shouldReturn` True
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.greatBarrow]
    stones <- locationOf Locations.standingStones
    run $ PlaceTokens GameSource (LocationTarget stones) Progress 4
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` map toCardCode [Locations.standingStones, Locations.greatBarrow]
    getModifiers home >>= (`shouldSatisfy` notElem CannotBeDefeated)
    run $ PlaceTokens GameSource home Progress 16
    getsGame (.outcome) `shouldReturn` Just Won

  it "reuses the same physical stage cards through repeated captures" . gameTestIn fogOnTheBarrowDowns testDeck $ \pid -> do
    home <- homeStage
    pool <- asideFours
    replicateM_ 6 do
      enterBarrow pid
      returnFromBarrow pid
      getCurrentQuest `shouldReturn` Just home
      sort <$> asideFours `shouldReturn` sort pool

  it "takes engaged enemies along and discards only the trapped player's excess allies" . twoPlayerTestIn fogOnTheBarrowDowns testDeck $ \one two -> do
    home <- homeStage
    discarded <- putAllyIntoPlay two Allies.guardOfTheCitadel
    kept <- putAllyIntoPlay two Allies.veteranAxehand
    other <- putAllyIntoPlay one Allies.guardOfTheCitadel
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    eid <- getsGame (toId . headEx . toList . (.entities.enemies))
    run $ PlaceEnemy eid (EngagedWith two)
    enterBarrow two
    chooseTarget two discarded
    getsGame (map toTarget . toList . (.entities.allies)) `shouldReturn` [kept]
    getsGame (map (.placement) . toList . (.entities.enemies)) >>= (`shouldSatisfy` elem (EngagedWith two))
    run $ FocusArea one
    getCurrentQuest `shouldReturn` Just home
    getsGame (map toTarget . toList . (.entities.allies)) `shouldReturn` [other]

  it "returns two players separately to the same empty home, preserving its identity" . twoPlayerTestIn fogOnTheBarrowDowns testDeck $ \one two -> do
    home <- homeStage
    enterBarrow two
    privateTwo <- stageOf Quests.trappedInsideABarrow
    run $ FocusArea one
    enterBarrow one
    privateOne <- stageOf Quests.trappedInsideABarrow
    privateOne `shouldSatisfy` (/= privateTwo)
    run $ FocusArea two
    returnFromBarrow two
    getCurrentQuest `shouldReturn` Just home
    getLead `shouldReturn` two
    getsGame (length . (.players)) `shouldReturn` 1
    run $ FocusArea one
    getCurrentQuest `shouldReturn` Just privateOne
    returnFromBarrow one
    getCurrentQuest `shouldReturn` Just home
    getLead `shouldReturn` one
    getsGame (length . (.players)) `shouldReturn` 2
    getsGame (isNothing . gameSeparation) `shouldReturn` True

  it "cancels an Ancient Barrow before its entry effect, and uses its dynamic threat" . gameTestIn fogOnTheBarrowDowns testDeck $ \pid -> do
    _ <- homeStage
    enterBarrow pid
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    initial <- getThreat pid
    _ <- setEncounterDeck [Locations.ancientBarrow, Locations.oldForestRoad]
    run RevealEncounterCard
    resolveRevealed
    getThreat pid `shouldReturn` initial + 1
    getsGame (map toTarget . toList . (.entities.allies)) `shouldReturn` [ally]
    getsGame (null . (.entities.locations)) `shouldReturn` True
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.ancientBarrow]
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.oldForestRoad]

  it "cancels another Great Barrow at stage 4 instead of creating another area" . gameTestIn fogOnTheBarrowDowns testDeck $ \pid -> do
    _ <- homeStage
    enterBarrow pid
    initial <- getThreat pid
    _ <- setEncounterDeck [Locations.greatBarrow, Locations.oldForestRoad]
    run RevealEncounterCard
    resolveRevealed
    getThreat pid `shouldReturn` initial + 1
    length <$> asideFours `shouldReturn` 3
    getsGame (maybe 0 (length . separationAreas) . gameSeparation) `shouldReturn` 2

  it "blocks progress at stage 3 while a Great Barrow remains in play" . twoPlayerTestIn fogOnTheBarrowDowns testDeck $ \one two -> do
    home@(QuestTarget qid) <- homeStage
    enterBarrow two
    run $ FocusArea one
    run $ PlaceTokens GameSource home Progress 8
    progressOn home `shouldReturn` 0
    getModifiers home >>= (`shouldSatisfy` elem (ProgressLimit 0))
    run $ FocusQuestArea qid
    getsGame (.outcome) `shouldReturn` Nothing

  for_ [("first", False), ("last", True)] $ \(position, capturedLast) ->
    it ("ends only the " <> position <> " revealing player's quest phase while the other player resolves the quest") . twoPlayerTestIn fogOnTheBarrowDowns testDeck $ \one two -> do
      let (captured, continuing) = if capturedLast then (two, one) else (one, two)
      home <- homeStage
      firstHero <- heroOf captured "01007"
      secondHero <- heroOf continuing "01007"
      current <- getGame
      putGame $ current & phaseL .~ QuestPhase
      run $ CreateEffect $ Effect GameSource firstHero [WillpowerModifier 2] UntilEndOfPhase
      run $ CreateEffect $ Effect GameSource secondHero [WillpowerModifier 2] UntilEndOfPhase
      run $ CommitToQuest firstHero
      run $ CommitToQuest secondHero
      initialOne <- getThreat captured
      initialTwo <- getThreat continuing
      _ <- setEncounterDeck $ if capturedLast
        then [Locations.oldForestRoad, Locations.greatBarrow, Enemies.coldWight, Locations.oldForestRoad]
        else [Locations.greatBarrow, Enemies.coldWight, Locations.oldForestRoad, Locations.oldForestRoad]
      pushAll [Staging, QuestResolution]
      void mainLoop
      resolveRevealed
      getCurrentQuest `shouldReturn` Just home
      getThreat continuing >>= (`shouldSatisfy` (> initialTwo))
      getModifiers secondHero >>= (`shouldSatisfy` elem (WillpowerModifier 2))
      run $ FocusArea captured
      getThreat captured `shouldReturn` initialOne
      getModifiers firstHero >>= (`shouldSatisfy` notElem (WillpowerModifier 2))
      case firstHero of
        HeroTarget hid -> heroCommitted . toAttrs <$> getHero hid `shouldReturn` False
        _ -> error "not a hero"
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.trappedInsideABarrow

  it "pays the Standing Stones travel cost from victory and still activates the Stones in the empty original area" . gameTestIn fogOnTheBarrowDowns testDeck $ \pid -> do
    home <- homeStage
    barrow <- genEncounterCard Locations.greatBarrow
    run $ AddToEncounterDiscard [barrow]
    run $ AddToVictoryDisplay (CardIdTarget barrow.id)
    _ <- setEncounterDeck [Enemies.coldWight, Locations.oldForestRoad, Locations.oldForestRoad]
    stones <- locationOf Locations.standingStones
    current <- getGame
    putGame $ current & phaseL .~ TravelPhase
    run $ TravelTo stones
    chooseTarget pid (CardIdTarget barrow.id)
    resolveRevealed
    getCurrentQuest `shouldReturn` Just home
    getsGame (null . (.players)) `shouldReturn` True
    (.placement) <$> getLocation stones `shouldReturn` ActiveLocation
    getsGame (any ((== barrow.id) . (.cardId) . toAttrs) . toList . (.entities.locations)) `shouldReturn` True
    getsGame (null . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` True
    run $ FocusArea pid
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.trappedInsideABarrow

  it "cannot travel to Standing Stones if no Great Barrow can pay its cost" . gameTestIn fogOnTheBarrowDowns testDeck $ \_ -> do
    _ <- homeStage
    _ <- setEncounterDeck [Locations.oldForestRoad]
    stones <- locationOf Locations.standingStones
    getModifiers (LocationTarget stones) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ TravelTo stones
    (.placement) <$> getLocation stones `shouldReturn` StagingArea
    getsGame (isNothing . gameSeparation) `shouldReturn` True

  it "removes a Great Barrow and raises only home-stage players' threat when a trapped player is eliminated" . twoPlayerTestIn fogOnTheBarrowDowns testDeck $ \one two -> do
    home <- homeStage
    enterBarrow two
    run $ FocusArea one
    initial <- getThreat one
    run $ FocusArea two
    run $ EliminatePlayer two
    finishBarrowChoice one
    run $ FocusArea one
    getCurrentQuest `shouldReturn` Just home
    getThreat one `shouldReturn` initial + 10
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.greatBarrow]
    length <$> asideFours `shouldReturn` 4

  it "retains stage 3 and its locations when its final player is eliminated" . twoPlayerTestIn fogOnTheBarrowDowns testDeck $ \one two -> do
    home@(QuestTarget qid) <- homeStage
    enterBarrow two
    run $ FocusArea one
    stones <- locationOf Locations.standingStones
    run $ EliminatePlayer one
    run $ FocusQuestArea qid
    getCurrentQuest `shouldReturn` Just home
    void $ getLocation stones
    run $ FocusArea two
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.trappedInsideABarrow
    getsGame (.outcome) `shouldReturn` Nothing
