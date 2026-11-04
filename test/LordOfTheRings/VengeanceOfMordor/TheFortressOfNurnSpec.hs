module LordOfTheRings.VengeanceOfMordor.TheFortressOfNurnSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (AllyType), toCardDef)
import LordOfTheRings.Decklist (oversizedDecks)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithType), getCurrentQuest)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theFortressOfNurn)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, powerOfMordorDeckL, questDeckL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

nurnTest :: (PlayerId -> GameT ()) -> IO ()
nurnTest = gameTestIn theFortressOfNurn testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

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
placementOfEnemy eid = getsGame (maybe Unplaced (.placement) . lookup eid . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

-- | A side quest added to the staging area.
sideQuest :: CardDef -> GameT QuestId
sideQuest def = do
  run . AddToStaging =<< genEncounterCard def
  questOf def

questOf :: CardDef -> GameT QuestId
questOf def = getsGame (fromJustNote "no such quest" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questAttrs :: QuestId -> GameT QuestAttrs
questAttrs qid = getsGame (fromJustNote "quest left play" . fmap toAttrs . lookup qid . (.entities.quests))

-- | A stage put into play on its B side.
stageB :: CardDef -> GameT QuestId
stageB def = do
  _ <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> if toCardCode a == toCardCode def then a {questFlipped = True} else a)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  questOf def

-- | N Castle side quests in the victory display.
castlesInVictory :: Int -> GameT ()
castlesInVictory n = do
  cards <- traverse genEncounterCard (take n [Quests.stormTheCastleCastleGarrison, Quests.stormTheCastleLethalCounterattack, Quests.stormTheCastleNoQuarter, Quests.stormTheCastleStrongholdOfMordor])
  g <- (scenarioL %~ overAttrs (victoryDisplayL .~ map EncounterCard cards)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

setPowerOfMordorDeck :: [CardDef] -> GameT ()
setPowerOfMordorDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (powerOfMordorDeckL .~ cards)

-- | Player cards of the player's, placed under the quest.
placeUnder :: PlayerId -> QuestId -> [CardDef] -> GameT [Card]
placeUnder pid qid defs = do
  cards <- traverse (fmap PlayerCard . (`genPlayerCard` Just pid)) defs
  run $ PlaceUnderneath (QuestTarget qid) cards
  pure cards

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

deckSize :: PlayerId -> GameT Int
deckSize pid = length . (.deck) <$> getPlayer pid

setHand :: PlayerId -> [Card] -> GameT ()
setHand pid cards = putGame . (playersL . ix pid . handL .~ cards) =<< getGame

-- | Read the modifiers again, after the test changed the game.
refresh :: GameT ()
refresh = putGame =<< preloadModifiers =<< getGame

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The enemy attacks the player, a card with no shadow effect dealt to it, and the
-- defender declares against it.
attackedBy :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
attackedBy pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

spec :: Spec
spec = describe "The Fortress of Nurn" do
  it "is a scenario the game can be dealt" . nurnTest $ \_ ->
    isJust (lookupScenario "the-fortress-of-nurn") `shouldBe` True

  -- \* Setup

  it "sets up as stage 1A reads" . nurnTest $ \pid -> do
    flipFirstQuest pid
    attrs <- scenarioAttrs
    -- the Power of Mordor deck, less the card under each Storm the Castle
    length attrs.powerOfMordorDeck `shouldBe` 1
    guards <- getsGame (filter ((== toCardCode Enemies.ulchorsGuard) . toCardCode) . toList . (.entities.enemies))
    map (.placement) guards `shouldBe` [EngagedWith pid]
    (placementOfEnemy =<< enemyOf Enemies.thaneUlchorTheFortressOfNurn) `shouldReturn` StagingArea
    quests <- getsGame (map toAttrs . toList . (.entities.quests))
    let storms = filter ((/= toCardCode Quests.assaultOnDolRhugar) . toCardCode) quests
    sort (map toCardCode storms)
      `shouldBe` sort (map toCardCode [Quests.stormTheCastleCastleGarrison, Quests.stormTheCastleLethalCounterattack, Quests.stormTheCastleNoQuarter, Quests.stormTheCastleStrongholdOfMordor])
    for_ storms \q -> do
      length [() | EncounterCard _ <- q.underneath] `shouldBe` 1
      length [() | PlayerCard _ <- q.underneath] `shouldBe` 8
    -- 8 under the quest deck too, from a 50-card deck less the 6 of the starting hand
    deckSize pid `shouldReturn` 4
    case attrs.meta of
      Object o | Just (Array under) <- KeyMap.lookup "underQuestDeck" o -> length under `shouldBe` 8
      _ -> liftIO $ expectationFailure "no cards under the quest deck"
    map toCardCode attrs.questDeck `shouldBe` [toCardCode Quests.theFinalConfrontation]
    let aside = map toCardCode [Enemies.thaneUlchorTheFortressOfNurn, Quests.stormTheCastleCastleGarrison, Enemies.khamulTheEasterling]
    any ((`elem` aside) . toCardCode) attrs.encounterDeck `shouldBe` False
    any ((== toCardCode Quests.theDungeonsOfDolRhugar) . toCardCode) attrs.encounterDeck `shouldBe` True
    -- The Fortress of Nurn, Ulchor's Guard and Under Guard, less the 5 set aside and the
    -- Ulchor's Guard engaged
    length attrs.encounterDeck `shouldBe` 40
    activeFlipped <- getsGame (any (\q -> toCardCode q == toCardCode Quests.assaultOnDolRhugar && (toAttrs q).flipped) . toList . (.entities.quests))
    activeFlipped `shouldBe` True

  it "builds the encounter deck with fewer copies in easy mode" $ gameTestIn (overAttrs (modeL .~ EasyMode) theFortressOfNurn) testDeck \_ -> do
    run (DoStep 1 Setup)
    deck <- (.encounterDeck) <$> scenarioAttrs
    let copies def = length (filter ((== toCardCode def) . toCardCode) deck)
    length deck `shouldBe` 28
    map copies [Enemies.easterlingMercenary, Enemies.mordorOlogHai, Enemies.guardDogs, Locations.fortressWallsTheFortressOfNurn, Locations.plazaOfNurn, Treacheries.theDarkLordsReach]
      `shouldBe` [1, 1, 1, 1, 2, 1]
    map copies [Enemies.soldierOfNurn, Locations.guardTower, Treacheries.determinedDefense, Quests.theDungeonsOfDolRhugar]
      `shouldBe` [3, 3, 3, 1]

  it "refuses a deck of more than 50 cards" . nurnTest $ \_ -> do
    let big = deckList testDeck.heroes (testDeck.cards <> [("01073", 1)])
    oversizedDecks "the-fortress-of-nurn" [testDeck] `shouldBe` Nothing
    oversizedDecks "the-fortress-of-nurn" [big] `shouldBe` Just (50, 51)
    oversizedDecks "passage-through-mirkwood" [big] `shouldBe` Nothing

  -- \* Assault on Dol Rhugar

  it "keeps Thane Ulchor in the staging area, and refuses to be chosen with 3 progress" . nurnTest $ \pid -> do
    qid <- stageB Quests.assaultOnDolRhugar
    t <- stage Enemies.thaneUlchorTheFortressOfNurn
    mods <- getModifiers (EnemyTarget t)
    mods `shouldSatisfy` elem CannotLeaveStagingArea
    run $ EngageEnemy pid t
    placementOfEnemy t `shouldReturn` StagingArea
    getModifiers (QuestTarget qid) >>= (`shouldSatisfy` notElem CannotBeCurrentQuest)
    run $ PlaceTokens GameSource (QuestTarget qid) Progress 3
    getModifiers (QuestTarget qid) >>= (`shouldSatisfy` elem CannotBeCurrentQuest)

  it "is defeated only at the end of the round with 3 Castle side quests in the victory display" . nurnTest $ \pid -> do
    final <- genEncounterCard Quests.theFinalConfrontation
    putGame . (scenarioL %~ overAttrs (questDeckL .~ [final])) =<< getGame
    qid <- stageB Quests.assaultOnDolRhugar
    run $ PlaceTokens GameSource (QuestTarget qid) Progress 5
    (.progress) <$> questAttrs qid `shouldReturn` 5
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> questAttrs qid `shouldReturn` 5
    castlesInVictory 3
    run $ PlaceTokens GameSource (QuestTarget qid) Progress 1
    (.progress) <$> questAttrs qid `shouldReturn` 6
    run $ CheckWindows [mkWhen RoundEnds]
    quests <- getsGame (map toCardCode . toList . (.entities.quests))
    quests `shouldBe` [toCardCode Quests.theFinalConfrontation]
    -- side 2A waits for its When Revealed to be resolved
    Just (QuestionLabel l _ _ _) <- getQuestion pid
    l `shouldSatisfy` ("$label.resolveWhenRevealed" `isPrefixOf`)

  -- \* The Final Confrontation

  it "reveals the top Power of Mordor card, shuffles the discard pile in, and has Thane Ulchor attack each player" . nurnTest $ \pid -> do
    t <- stage Enemies.thaneUlchorTheFortressOfNurn
    setPowerOfMordorDeck [Locations.theTowerOfBaradDur]
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.oldForestRoad
    _ <- putQuestStage Quests.theFinalConfrontation
    run . Flip =<< (toTarget <$> getActiveQuest)
    resolveRevealed
    locationCodes `shouldReturn` [toCardCode Locations.theTowerOfBaradDur]
    getEncounterDiscard `shouldReturn` []
    -- the attack's shadow card came from the encounter deck, which had the discard pile
    -- shuffled in
    passActions pid
    Just (QuestionLabel _ source _ _) <- getQuestion pid
    source `shouldBe` Just (toTarget t)

  it "caps Thane Ulchor's damage at its progress, forbids canceling, and wins when he is defeated" . nurnTest $ \_ -> do
    qid <- stageB Quests.theFinalConfrontation
    t <- stage Enemies.thaneUlchorTheFortressOfNurn
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)
    run $ PlaceTokens GameSource (QuestTarget qid) Progress 3
    run $ PlaceTokens GameSource (EnemyTarget t) Damage 5
    getDamage (EnemyTarget t) `shouldReturn` 3
    -- the twelfth would defeat the stage, the last
    run $ PlaceTokens GameSource (QuestTarget qid) Progress 8
    run $ PlaceTokens GameSource (EnemyTarget t) Damage 20
    getDamage (EnemyTarget t) `shouldReturn` 11
    run $ Destroy GameSource (EnemyTarget t)
    getsGame (.outcome) `shouldReturn` Just Won

  -- \* Storm the Castle

  it "reveals the card under Storm the Castle as it becomes the current quest, then turns over: Lethal Counterattack" . nurnTest $ \_ -> do
    qid <- sideQuest Quests.stormTheCastleLethalCounterattack
    tower <- genEncounterCard Locations.theTowerOfBaradDur
    run $ PlaceUnderneath (QuestTarget qid) [EncounterCard tower]
    run $ SetCurrentQuest (QuestTarget qid)
    resolveRevealed
    locationCodes `shouldReturn` [toCardCode Locations.theTowerOfBaradDur]
    q <- questAttrs qid
    q.flipped `shouldBe` True
    q.underneath `shouldBe` []
    getCurrentQuest `shouldReturn` Just (QuestTarget qid)
    beravor <- hero "01012"
    getDamage beravor `shouldReturn` 1

  it "adds X enemies discarded to the staging area: Castle Garrison" . nurnTest $ \_ -> do
    qid <- sideQuest Quests.stormTheCastleCastleGarrison
    _ <- setEncounterDeck [Locations.oldForestRoad, Enemies.forestSpider, Enemies.forestSpider]
    run $ SetCurrentQuest (QuestTarget qid)
    resolveRevealed
    enemyCodes `shouldReturn` [toCardCode Enemies.forestSpider]
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.oldForestRoad]

  it "adds X locations discarded to the staging area: Stronghold of Mordor" . nurnTest $ \_ -> do
    qid <- sideQuest Quests.stormTheCastleStrongholdOfMordor
    _ <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad, Locations.oldForestRoad]
    run $ SetCurrentQuest (QuestTarget qid)
    resolveRevealed
    locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.forestSpider]

  it "discards 4 cards from under No Quarter and each card sharing their titles" . nurnTest $ \pid -> do
    qid <- sideQuest Quests.stormTheCastleNoQuarter
    _ <- placeUnder pid qid [toCardDef ("01012" :: CardCode), Allies.gandalf, Allies.gandalf, Allies.gandalf]
    gandalf <- putCardIntoHand pid Allies.gandalf
    faramir <- putCardIntoHand pid Allies.faramir
    run $ SetCurrentQuest (QuestTarget qid)
    resolveRevealed
    q <- questAttrs qid
    q.underneath `shouldBe` []
    hand <- (.hand) <$> getPlayer pid
    (gandalf `elem` hand, faramir `elem` hand) `shouldBe` (False, True)
    -- Beravor shares a title with the hero card discarded
    getsGame (any ((== "01012") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` False

  it "shuffles the player cards under it into their owners' decks when defeated, then each player draws 1" . nurnTest $ \pid -> do
    qid <- sideQuest Quests.stormTheCastleCastleGarrison
    _ <- setEncounterDeck []
    run $ SetCurrentQuest (QuestTarget qid)
    resolveRevealed
    _ <- placeUnder pid qid [Allies.gandalf, Allies.gandalf]
    hand <- handSize pid
    deck <- deckSize pid
    run $ PlaceTokens GameSource (QuestTarget qid) Progress 6
    handSize pid `shouldReturn` (hand + 1)
    deckSize pid `shouldReturn` (deck + 1)
    victoryCodes `shouldReturn` [toCardCode Quests.stormTheCastleCastleGarrison]

  -- \* The Dungeons of Dol Rhugar

  it "surges, limits the current quest to 4 progress a round, and gives back a card from under a Castle side quest" . nurnTest $ \pid -> do
    main <- stageB Quests.assaultOnDolRhugar
    castle <- sideQuest Quests.stormTheCastleNoQuarter
    [card, _] <- placeUnder pid castle [Allies.gandalf, Allies.faramir]
    reveal [Quests.theDungeonsOfDolRhugar, Locations.oldForestRoad]
    locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
    dungeons <- questOf Quests.theDungeonsOfDolRhugar
    run $ PlaceTokens GameSource (QuestTarget main) Progress 6
    (.progress) <$> questAttrs main `shouldReturn` 4
    run $ PlaceTokens GameSource (QuestTarget dungeons) Progress 4
    -- the one Castle side quest is chosen, and the player picks his card
    chooseTarget pid card
    hand <- (.hand) <$> getPlayer pid
    card `elem` hand `shouldBe` True
    q <- questAttrs castle
    length q.underneath `shouldBe` 1
    victoryCodes `shouldReturn` [toCardCode Quests.theDungeonsOfDolRhugar]

  -- \* Enemies

  it "makes Thane Ulchor immune with threat X, and reveals a card after he attacks" . nurnTest $ \pid -> do
    castlesInVictory 1
    t <- engaged pid Enemies.thaneUlchorTheFortressOfNurn
    getEnemyThreat t `shouldReturn` 2
    getModifiers (EnemyTarget t) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run $ DealShadowCard t
    inPhase CombatPhase
    run $ EnemyAttacks pid
    passActions pid
    chooseTarget pid =<< hero "01012"
    passActions pid
    locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]

  it "has the player place a card from hand under a Castle side quest after Soldier of Nurn attacks" . nurnTest $ \pid -> do
    castle <- sideQuest Quests.stormTheCastleNoQuarter
    card <- putCardIntoHand pid Allies.gandalf
    soldier <- engaged pid Enemies.soldierOfNurn
    attackedBy pid soldier Locations.oldForestRoad =<< hero "01012"
    chooseTarget pid card
    q <- questAttrs castle
    q.underneath `shouldBe` [card]

  it "gives +1 attack with Soldier of Nurn's shadow, and a card under a Castle side quest if the attack destroys a character" . nurnTest $ \pid -> do
    castle <- sideQuest Quests.stormTheCastleNoQuarter
    spider <- engaged pid Enemies.forestSpider
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    card <- putCardIntoHand pid Allies.gandalf
    attackedBy pid spider Enemies.soldierOfNurn ally
    chooseTarget pid card
    q <- questAttrs castle
    q.underneath `shouldBe` [card]

  it "has Easterling Mercenary reveal another card, or stop progress on the current quest" . nurnTest $ \pid -> do
    reveal [Enemies.easterlingMercenary, Locations.oldForestRoad]
    chooseLabel "$label.easterlingMercenary.reveal" pid
    resolveRevealed
    locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
    qid <- stageB Quests.assaultOnDolRhugar
    reveal [Enemies.easterlingMercenary]
    chooseLabel "$label.easterlingMercenary.noProgress" pid
    run $ PlaceTokens GameSource (QuestTarget qid) Progress 2
    (.progress) <$> questAttrs qid `shouldReturn` 0

  it "gives +1 attack for each Castle side quest in the victory display with Easterling Mercenary's shadow" . nurnTest $ \pid -> do
    castlesInVictory 2
    spider <- engaged pid Enemies.forestSpider
    beravor <- hero "01012"
    attackedBy pid spider Enemies.easterlingMercenary beravor
    -- Forest Spider's 2 attack, +1 for engaging this round, +2, against Beravor's 2 defense
    getDamage beravor `shouldReturn` 3

  it "has Mordor Olog-hai take no attachments, and exhaust a character after its attack destroys one" . nurnTest $ \pid -> do
    olog <- engaged pid Enemies.mordorOlogHai
    getModifiers (EnemyTarget olog) >>= (`shouldSatisfy` elem CannotHaveAttachments)
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    attackedBy pid olog Locations.oldForestRoad ally
    beravor <- hero "01012"
    chooseTarget pid beravor
    exhausted "01012" `shouldReturn` True

  it "gives +2 attack with Mordor Olog-hai's shadow" . nurnTest $ \pid -> do
    spider <- engaged pid Enemies.forestSpider
    beravor <- hero "01012"
    attackedBy pid spider Enemies.mordorOlogHai beravor
    -- Forest Spider's 2 attack, +1 for engaging this round, +2, against Beravor's 2 defense
    getDamage beravor `shouldReturn` 3

  it "has Guard Dogs surge and deal 1 damage as they engage, and come into play with their shadow" . nurnTest $ \pid -> do
    reveal [Enemies.guardDogs, Locations.oldForestRoad]
    locationCodes `shouldReturn` [toCardCode Locations.oldForestRoad]
    dogs <- enemyOf Enemies.guardDogs
    run $ EngageEnemy pid dogs
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1

  it "puts Guard Dogs into play engaged with the defending player with their shadow" . nurnTest $ \pid -> do
    spider <- engaged pid Enemies.forestSpider
    attackedBy pid spider Enemies.guardDogs =<< hero "01007"
    dogs <- enemyOf Enemies.guardDogs
    placementOfEnemy dogs `shouldReturn` EngagedWith pid

  -- \* Locations

  it "raises the cost of allies from the staging area, and places a card under a Castle side quest to travel to Fortress Walls" . nurnTest $ \pid -> do
    walls <- stageLocation Locations.fortressWallsTheFortressOfNurn
    getModifiers GameTarget >>= (`shouldSatisfy` elem (IncreaseCost (CardWithType AllyType) 1))
    getModifiers (LocationTarget walls) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    castle <- sideQuest Quests.stormTheCastleNoQuarter
    setHand pid []
    card <- putCardIntoHand pid Allies.gandalf
    refresh
    getModifiers (LocationTarget walls) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo walls
    chooseTarget pid card
    q <- questAttrs castle
    q.underneath `shouldBe` [card]

  it "stops draws and resources from player cards while Guard Tower is staged, and exhausts a character to travel there" . nurnTest $ \pid -> do
    tower <- stageLocation Locations.guardTower
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotDrawCardsFromPlayerCardEffects)
    beravor <- hero "01012"
    getModifiers beravor >>= (`shouldSatisfy` elem CannotGainResourcesFromPlayerCardEffects)
    run $ TravelTo tower
    chooseTarget pid beravor
    exhausted "01012" `shouldReturn` True

  it "has Plaza of Nurn's threat X, and deal X damage to travel there" . nurnTest $ \pid -> do
    plaza <- stageLocation Locations.plazaOfNurn
    getLocationThreat plaza `shouldReturn` 1
    castlesInVictory 2
    getLocationThreat plaza `shouldReturn` 3
    castlesInVictory 0
    run $ TravelTo plaza
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1

  -- \* Treacheries

  it "adds 2 threat, and 2 more for each Castle side quest in the victory display, with Determined Defense" . nurnTest $ \_ -> do
    castlesInVictory 1
    reveal [Treacheries.determinedDefense]
    getStagingThreat `shouldReturn` 4

  it "exhausts a character with Determined Defense's shadow" . nurnTest $ \pid -> do
    spider <- engaged pid Enemies.forestSpider
    _ <- setEncounterDeck [Treacheries.determinedDefense]
    run $ DealShadowCard spider
    inPhase CombatPhase
    run $ EnemyAttacks pid
    passActions pid
    chooseTarget pid =<< hero "01007"
    passActions pid
    chooseTarget pid =<< hero "01012"
    exhausted "01012" `shouldReturn` True

  it "damages each exhausted character and discards an attachment from each with The Dark Lord's Reach" . nurnTest $ \pid -> do
    beravor <- hero "01012"
    blade <- putAttachmentOn pid Attachments.bladeOfGondolin beravor
    run $ Exhaust beravor
    reveal [Treacheries.theDarkLordsReach]
    getDamage beravor `shouldReturn` 1
    getsGame (any ((== blade) . toTarget) . toList . (.entities.attachments)) `shouldReturn` False

  it "discards an attachment the defending player controls with The Dark Lord's Reach's shadow" . nurnTest $ \pid -> do
    eowyn <- hero "01007"
    blade <- putAttachmentOn pid Attachments.bladeOfGondolin eowyn
    spider <- engaged pid Enemies.forestSpider
    attackedBy pid spider Treacheries.theDarkLordsReach =<< hero "01012"
    getsGame (any ((== blade) . toTarget) . toList . (.entities.attachments)) `shouldReturn` False
