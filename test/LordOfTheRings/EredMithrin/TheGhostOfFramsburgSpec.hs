module LordOfTheRings.EredMithrin.TheGhostOfFramsburgSpec (spec) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (LocationType))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getThreat)
import LordOfTheRings.Message.Lifted (lowerThreatBy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theGhostOfFramsburg)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), encounterDiscardL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins, PlayedCard), mkAfter)
import TestImport

-- | The test deck's heroes start the player at 27 threat once the game is set up; before
-- it is, the threat is raised to that.
framsburgTest :: (PlayerId -> GameT ()) -> IO ()
framsburgTest body = gameTestIn theGhostOfFramsburg testDeck \pid -> run (RaiseThreat pid 27) >> body pid

objectives :: GameT [ObjectiveAttrs]
objectives = getsGame (map toAttrs . toList . (.entities.objectives))

objectiveOf :: CardDef -> GameT ObjectiveAttrs
objectiveOf def = fromJustNote "no such objective" . find ((== toCardCode def) . objectiveCardCode) <$> objectives

enemies :: GameT [EnemyAttrs]
enemies = getsGame (map toAttrs . toList . (.entities.enemies))

enemyOf :: CardDef -> GameT EnemyAttrs
enemyOf def = fromJustNote "no such enemy" . find ((== toCardCode def) . toCardCode) <$> enemies

locationOf :: CardDef -> GameT LocationAttrs
locationOf def = getsGame (fromJustNote "no such location" . find ((== toCardCode def) . toCardCode) . map toAttrs . toList . (.entities.locations))

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

staged :: CardDef -> GameT LocationId
staged def = stage def >> (.id) <$> locationOf def

hero :: GameT Target
hero = toTarget <$> getHeroWithCode "01012"

-- | The Loot objectives, put straight onto the hero, as claimed.
onHero :: [CardDef] -> GameT ()
onHero defs = do
  h <- hero
  for_ defs \def -> run . (`PutObjectiveIntoPlayAttached` h) =<< genEncounterCard def

setDiscard :: [CardDef] -> GameT ()
setDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)

setAsideCards :: [CardDef] -> GameT ()
setAsideCards defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

codes :: [EncounterCard] -> [CardCode]
codes = map toCardCode

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

flipped :: CardDef -> Int -> GameT Target
flipped def n = do
  q <- putQuestStage def
  run $ Flip q
  when (n > 0) $ run $ PlaceTokens GameSource q Progress n
  pure q

-- | A non-Hazard filler card for the encounter deck.
filler :: CardDef
filler = Enemies.giantSpider

spec :: Spec
spec = describe "The Ghost of Framsburg" do
  it "is a scenario the game can be dealt" . framsburgTest $ \_ ->
    isJust (lookupScenario "the-ghost-of-framsburg") `shouldBe` True

  -- \* Setup

  it "sets The Cursed Tomb aside and has each player search for a location as stage 1 is set up" . framsburgTest $ \pid -> do
    flipFirstQuest pid
    chooseOptionMatching "search for a location" pid (const True)
    map toCardCode . scenarioSetAside . toAttrs <$> getsGame (.scenario) `shouldReturn` [toCardCode Locations.theCursedTomb]
    ls <- getsGame (map toAttrs . toList . (.entities.locations))
    map (cdCardType . toCardDef) ls `shouldBe` [LocationType]
    map (.placement) ls `shouldBe` [StagingArea]
    -- 42 cards in all: The Cursed Tomb aside, 1 location in the staging area
    length <$> getEncounterDeck `shouldReturn` 40
    getActiveQuest >>= (`shouldSatisfy` (.flipped)) . toAttrs

  -- \* Discover and Loot

  it "resolves Discover as the location becomes active: a Loot objective it guards, a Hazard revealed, the rest discarded" . framsburgTest $ \pid -> do
    hall <- staged Locations.hauntedHall
    _ <- setEncounterDeck [Objectives.cryptKey, filler, Enemies.evilSpectre, Treacheries.afraidOfTheDark, Treacheries.eyesInTheDark]
    run $ TravelTo hall
    -- with no Loot to choose between, the player is shown the cards he looked at
    q <- fromJustNote "no cards shown" <$> getQuestion pid
    map toCardCode (searchedCards q) `shouldBe` map toCardCode [Objectives.cryptKey, filler, Enemies.evilSpectre, Treacheries.afraidOfTheDark]
    chooseLabel "$label.continue" pid
    hallCard <- (.cardId) <$> locationOf Locations.hauntedHall
    key <- objectiveOf Objectives.cryptKey
    key.placement `shouldBe` StagingArea
    objectiveGuard key `shouldBe` Just hallCard
    (.placement) <$> enemyOf Enemies.evilSpectre `shouldReturn` StagingArea
    codes <$> getEncounterDiscard `shouldReturn` [toCardCode filler, toCardCode Treacheries.afraidOfTheDark]
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Treacheries.eyesInTheDark]
    -- explored, it guards nothing: the first player claims the objective
    run $ PlaceTokens GameSource (LocationTarget hall) Progress 4
    h <- hero
    chooseTarget pid h
    (.placement) <$> objectiveOf Objectives.cryptKey `shouldReturn` AttachedTo h

  it "has the first player choose 1 of several Loot objectives, discarding the other" . framsburgTest $ \pid -> do
    passage <- staged Locations.moonlitPassage
    [_, lantern] <- setEncounterDeck [Objectives.cryptKey, Objectives.oldLantern]
    run $ TravelTo passage
    chooseTarget pid lantern
    map objectiveCardCode <$> objectives `shouldReturn` [toCardCode Objectives.oldLantern]
    codes <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.cryptKey]

  it "shuffles the discard pile in when the deck runs short of the Discover value" . framsburgTest $ \pid -> do
    passage <- staged Locations.moonlitPassage
    _ <- setEncounterDeck [filler]
    setDiscard [Objectives.scathasTeeth]
    run $ TravelTo passage
    chooseLabel "$label.continue" pid
    map objectiveCardCode <$> objectives `shouldReturn` [toCardCode Objectives.scathasTeeth]
    codes <$> getEncounterDiscard `shouldReturn` [toCardCode filler]
    getEncounterDeck `shouldReturn` []

  it "discards a Loot objective revealed from the encounter deck and reveals another card" . framsburgTest $ \_ -> do
    _ <- setEncounterDeck [Objectives.oldLantern, filler]
    run RevealEncounterCard
    codes <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.oldLantern]
    map toCardCode <$> enemies `shouldReturn` [toCardCode filler]
    objectives `shouldReturn` []

  -- \* The Loot objectives

  it "Crypt Key raises the Discover value of a location the players travel to by 1" . framsburgTest $ \pid -> do
    onHero [Objectives.cryptKey]
    tower <- staged Locations.cursedTower
    _ <- setEncounterDeck [filler, Enemies.werewolf, Treacheries.afraidOfTheDark, Treacheries.eyesInTheDark, Enemies.blackBats]
    run $ TravelTo tower
    chooseAbilityOf pid . ObjectiveTarget . objectiveId =<< objectiveOf Objectives.cryptKey
    chooseLabelStarting "$label.cryptKey.increase" pid
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.blackBats]
    (.exhausted) <$> objectiveOf Objectives.cryptKey `shouldReturn` True
    -- spent by the resolution
    getModifiers (LocationTarget tower) >>= (`shouldSatisfy` notElem (DiscoverValueModifier 1))

  it "Old Lantern adds one of the top X cards of a player's deck to his hand, X the active location's Discover value" . framsburgTest $ \pid -> do
    onHero [Objectives.oldLantern]
    _ <- setEncounterDeck []
    passage <- staged Locations.moonlitPassage
    run $ TravelTo passage
    deck <- (.deck) <$> getPlayer pid
    hand <- length . (.hand) <$> getPlayer pid
    lantern <- objectiveId <$> objectiveOf Objectives.oldLantern
    run $ UseCardAbility pid (ObjectiveSource lantern) 1 []
    case deck of
      a : b : _ -> do
        chooseTarget pid b
        p <- getPlayer pid
        length p.hand `shouldBe` hand + 1
        map (.id) (take 1 $ reverse p.deck) `shouldBe` [a.id]
      _ -> error "a deck of 2 cards or more"

  it "Scatha's Teeth reduces a player's threat by 2" . framsburgTest $ \pid -> do
    onHero [Objectives.scathasTeeth]
    teeth <- objectiveId <$> objectiveOf Objectives.scathasTeeth
    run $ UseCardAbility pid (ObjectiveSource teeth) 1 []
    getThreat pid `shouldReturn` 25

  -- \* The locations

  it "bars travel to The Cursed Tomb until the players control 3 Loot objectives" . framsburgTest $ \_ -> do
    tomb <- staged Locations.theCursedTomb
    onHero [Objectives.cryptKey, Objectives.oldLantern]
    getModifiers (LocationTarget tomb) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    onHero [Objectives.scathasTeeth]
    getModifiers (LocationTarget tomb) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    getModifiers (LocationTarget tomb) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  it "brings out Fram's Shade to attack as the players travel to The Cursed Tomb" . framsburgTest $ \pid -> do
    tomb <- staged Locations.theCursedTomb
    onHero [Objectives.cryptKey, Objectives.oldLantern, Objectives.scathasTeeth]
    _ <- setEncounterDeck [filler, Enemies.framsShade]
    run $ TravelTo tomb
    chooseOptionMatching "search for Fram's Shade" pid (const True)
    (.placement) <$> enemyOf Enemies.framsShade `shouldReturn` StagingArea
    -- its attack has begun: the player is asked to defend it
    isJust <$> getQuestion pid `shouldReturn` True

  it "Forgotten Crypt finds an Undead enemy to guard a Loot objective from the discard pile" . framsburgTest $ \_ -> do
    crypt <- staged Locations.forgottenCrypt
    _ <- setEncounterDeck [filler, Enemies.evilSpectre, Enemies.werewolf]
    setDiscard [Objectives.scathasTeeth]
    run $ PlaceLocation crypt ActiveLocation
    spectre <- enemyOf Enemies.evilSpectre
    spectre.placement `shouldBe` StagingArea
    objectiveGuard <$> objectiveOf Objectives.scathasTeeth `shouldReturn` Just spectre.cardId
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.werewolf]

  it "Haunted Hall raises the threat of a player whose card places progress on a Dark location in the staging area" . framsburgTest $ \pid -> do
    _ <- staged Locations.hauntedHall
    tower <- staged Locations.cursedTower
    h <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (LocationTarget tower) Progress 1
    getThreat pid `shouldReturn` 27
    run $ PlaceTokens (HeroSource h.id) (LocationTarget tower) Progress 1
    getThreat pid `shouldReturn` 28

  it "Hidden Staircase has X threat and Discover X, and reduces each player's threat by X when it reveals no Hazard" . framsburgTest $ \pid -> do
    stairs <- staged Locations.hiddenStaircase
    getLocationThreat stairs `shouldReturn` 2
    _ <- setEncounterDeck [filler, Enemies.werewolf, Enemies.blackBats]
    run $ TravelTo stairs
    chooseLabel "$label.continue" pid
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.blackBats]
    getThreat pid `shouldReturn` 25

  it "Hidden Staircase reduces no threat when its Discover reveals a Hazard" . framsburgTest $ \pid -> do
    stairs <- staged Locations.hiddenStaircase
    _ <- setEncounterDeck [Enemies.evilSpectre, filler]
    run $ TravelTo stairs
    chooseLabel "$label.continue" pid
    (.placement) <$> enemyOf Enemies.evilSpectre `shouldReturn` StagingArea
    getThreat pid `shouldReturn` 27

  it "Moonlit Passage lets player cards reduce each player's threat by no more than 1 each round" . framsburgTest $ \pid -> do
    _ <- staged Locations.moonlitPassage
    h <- getHeroWithCode "01012"
    runQueueT $ lowerThreatBy (HeroSource h.id) pid 3
    run $ Run []
    getThreat pid `shouldReturn` 26
    runQueueT $ lowerThreatBy (HeroSource h.id) pid 3
    run $ Run []
    getThreat pid `shouldReturn` 26
    runQueueT $ lowerThreatBy GameSource pid 2
    run $ Run []
    getThreat pid `shouldReturn` 24

  it "Cursed Tower raises the threat of a player who plays an event" . framsburgTest $ \pid -> do
    _ <- staged Locations.cursedTower
    event <- genPlayerCard Events.feint (Just pid)
    run $ CheckWindows [mkAfter $ PlayedCard pid event]
    getThreat pid `shouldReturn` 28

  -- \* The enemies

  it "Fram's Shade grows with each Loot objective the players control, and is dealt that many shadow cards" . framsburgTest $ \_ -> do
    stage Enemies.framsShade
    shade <- (.id) <$> enemyOf Enemies.framsShade
    onHero [Objectives.cryptKey, Objectives.oldLantern]
    getEnemyThreat shade `shouldReturn` 5
    getEnemyAttack shade `shouldReturn` 5
    getEnemyDefense shade `shouldReturn` 5
    _ <- setEncounterDeck [filler, Enemies.werewolf, Enemies.blackBats]
    run $ DealShadowCard shade
    length . enemyShadowCards <$> enemyOf Enemies.framsShade `shouldReturn` 2

  it "deals Fram's Shade 1 shadow card while the players control no Loot objective" . framsburgTest $ \_ -> do
    stage Enemies.framsShade
    shade <- (.id) <$> enemyOf Enemies.framsShade
    _ <- setEncounterDeck [filler, Enemies.werewolf]
    run $ DealShadowCard shade
    length . enemyShadowCards <$> enemyOf Enemies.framsShade `shouldReturn` 1

  it "Crypt Stalker's threat, attack and defense are the tens digit of the first player's threat" . framsburgTest $ \_ -> do
    stage Enemies.cryptStalker
    stalker <- (.id) <$> enemyOf Enemies.cryptStalker
    getEnemyThreat stalker `shouldReturn` 2
    getEnemyAttack stalker `shouldReturn` 2
    getEnemyDefense stalker `shouldReturn` 2

  it "shuffles Evil Spectre back into the encounter deck as it is destroyed while the active location is Dark" . framsburgTest $ \_ -> do
    _ <- setEncounterDeck []
    hall <- staged Locations.hauntedHall
    run $ PlaceLocation hall ActiveLocation
    stage Enemies.evilSpectre
    spectre <- (.id) <$> enemyOf Enemies.evilSpectre
    run $ Destroy GameSource (EnemyTarget spectre)
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.evilSpectre]

  -- \* The treacheries

  it "Abandoned Ruins has each player resolve the active location's Discover keyword" . framsburgTest $ \_ -> do
    _ <- setEncounterDeck []
    hall <- staged Locations.hauntedHall
    run $ PlaceLocation hall ActiveLocation
    _ <- setEncounterDeck [filler, Enemies.werewolf, Enemies.blackBats, Treacheries.eyesInTheDark, Treacheries.afraidOfTheDark]
    run . RevealCard =<< genEncounterCard Treacheries.abandonedRuins
    resolveRevealed
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Treacheries.afraidOfTheDark]

  it "Abandoned Ruins gains surge and doomed 2 with no Discover location active" . framsburgTest $ \pid -> do
    _ <- setEncounterDeck [filler]
    run . RevealCard =<< genEncounterCard Treacheries.abandonedRuins
    resolveRevealed
    getThreat pid `shouldReturn` 29
    map toCardCode <$> enemies `shouldReturn` [toCardCode filler]

  it "Terrible Shadows gives each character -1 willpower, attack and defense" . framsburgTest $ \_ -> do
    h <- hero
    run . RevealCard =<< genEncounterCard Treacheries.terribleShadows
    resolveRevealed
    getModifiers h >>= (`shouldSatisfy` \ms -> all (`elem` ms) [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)])

  it "Slammed Shut exhausts a character and stops player cards readying characters this round" . framsburgTest $ \pid -> do
    h <- hero
    run . RevealCard =<< genEncounterCard Treacheries.slammedShut
    resolveRevealed
    chooseTarget pid h
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotBeReadiedByPlayerCards)
    -- doomed 1
    getThreat pid `shouldReturn` 28

  -- \* The stages

  it "blocks stage 1 until the players control a Loot objective" . framsburgTest $ \_ -> do
    q <- flipped Quests.theHauntedKeep 0
    getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
    onHero [Objectives.cryptKey]
    getModifiers q >>= (`shouldSatisfy` notElem CannotBeDefeated)

  it "Searching the Ruins shuffles the discard pile in and reveals a card for each player, then needs 2 Loot objectives" . framsburgTest $ \pid -> do
    _ <- setEncounterDeck []
    setDiscard [filler]
    q <- putQuestStage Quests.searchingTheRuins
    run $ Flip q
    resolveWhenRevealed pid
    map toCardCode <$> enemies `shouldReturn` [toCardCode filler]
    onHero [Objectives.cryptKey]
    getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
    onHero [Objectives.oldLantern]
    getModifiers q >>= (`shouldSatisfy` notElem CannotBeDefeated)

  it "Searching the Ruins has the first player raise each player's threat by 2 or find a location as the quest phase begins" . framsburgTest $ \pid -> do
    _ <- setEncounterDeck []
    _ <- flipped Quests.searchingTheRuins 0
    resolveWhenRevealed pid
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    chooseLabelStarting "$label.searchingTheRuins.raiseThreat" pid
    getThreat pid `shouldReturn` 29
    _ <- setEncounterDeck [filler, Locations.cursedTower, Enemies.werewolf]
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    chooseLabelStarting "$label.searchingTheRuins.location" pid
    (.placement) <$> locationOf Locations.cursedTower `shouldReturn` StagingArea
    codes <$> getEncounterDiscard `shouldReturn` [toCardCode filler]

  it "The Cursed Shade adds The Cursed Tomb, and is won once Fram's Shade, worth 6 with the Tomb in the victory display, is there" . framsburgTest $ \pid -> do
    setAsideCards [Locations.theCursedTomb]
    q <- putQuestStage Quests.theCursedShade
    run $ Flip q
    resolveWhenRevealed pid
    tomb <- (.id) <$> locationOf Locations.theCursedTomb
    stage Enemies.framsShade
    shade <- (.id) <$> enemyOf Enemies.framsShade
    getModifiers (EnemyTarget shade) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
    run $ AddToVictoryDisplay (LocationTarget tomb)
    getModifiers (EnemyTarget shade) >>= (`shouldSatisfy` elem (GainVictory 6))
    run $ PlaceTokens GameSource q Progress 6
    getsGame (.outcome) `shouldReturn` Nothing
    run $ Destroy GameSource (EnemyTarget shade)
    s <- getsGame (toAttrs . (.scenario))
    map toCardCode s.victoryDisplay `shouldSatisfy` elem (toCardCode Enemies.framsShade)
    s.victoryPoints `shouldBe` 11
    getsGame (.outcome) `shouldReturn` Just Won
