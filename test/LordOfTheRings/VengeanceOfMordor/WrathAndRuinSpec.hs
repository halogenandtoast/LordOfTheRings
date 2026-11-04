module LordOfTheRings.VengeanceOfMordor.WrathAndRuinSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdKeywords))
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getStagingThreat)
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Keyword (Keyword (Archery, Surge))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, wrathAndRuin)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), encounterDiscardL, modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

warTest :: (PlayerId -> GameT ()) -> IO ()
warTest = gameTestIn wrathAndRuin testDeck

-- | A table of two players, both with the test deck.
warTable :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
warTable body = do
  seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", testDeck)])
  env <- newGameEnv (newTableIn wrathAndRuin seats) (const $ pure ()) 1
  runGameT env do
    order <- getsGame (.playerOrder)
    case order of
      [one, two] -> body one two
      _ -> error "two players"

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOf :: LocationId -> GameT Placement
placementOf lid = (.placement) <$> getLocation lid

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = getsGame (maybe Unplaced (.placement) . lookup eid . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

-- | Put the location into play in the staging area.
stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

-- | Put the location into play under the player's control.
controlled :: PlayerId -> CardDef -> GameT LocationId
controlled pid def = do
  lid <- stageLocation def
  run $ PlaceLocation lid (InPlayArea pid)
  pure lid

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

-- | A stage put into play on its B side.
stageB :: CardDef -> GameT Target
stageB def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Cards on top of the encounter discard pile, the first on top.
inDiscard :: [CardDef] -> GameT ()
inDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)

discardCodes :: GameT [CardCode]
discardCodes = map toCardCode <$> getEncounterDiscard

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

shadowAttack :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
shadowAttack pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

-- | The source of the question the player is asked, as an attack's defender question
-- names the attacking enemy.
questionSource :: PlayerId -> GameT (Maybe Target)
questionSource pid =
  getQuestion pid <&> \case
    Just (QuestionLabel _ source _ _) -> source
    _ -> Nothing

roundEnds :: GameT ()
roundEnds = run $ CheckWindows [mkWhen RoundEnds]

spec :: Spec
spec = describe "Wrath and Ruin" do
  it "is a scenario the game can be dealt" . warTest $ \_ ->
    isJust (lookupScenario "wrath-and-ruin") `shouldBe` True

  -- \* Setup

  it "builds the encounter deck from Wrath and Ruin, Easterling Raiders and City of Rhûn, with Drive them Out to follow" . warTest $ \_ -> do
    run Setup
    attrs <- scenarioAttrs
    let codes = map toCardCode attrs.encounterDeck
        copies def = length (filter (== toCardCode def) codes)
    copies Enemies.easterlingSkirmisher `shouldBe` 4
    copies Locations.contestedSquare `shouldBe` 3
    copies Enemies.easterlingRaider `shouldBe` 3
    copies Locations.cityForum `shouldBe` 2
    map toCardCode attrs.questDeck `shouldBe` [toCardCode Quests.driveThemOut]

  it "keeps easy mode's quantities" . gameTestIn (overAttrs (modeL .~ EasyMode) wrathAndRuin) testDeck $ \_ -> do
    run Setup
    codes <- map toCardCode . (.encounterDeck) <$> scenarioAttrs
    let copies def = length (filter (== toCardCode def) codes)
    map copies [Enemies.easterlingSkirmisher, Locations.contestedSquare, Locations.makeshiftFortification, Locations.capturedTower]
      `shouldBe` [2, 2, 2, 1]
    map copies [Treacheries.fury, Treacheries.caughtOffGuard, Treacheries.ulchorsRevenge, Treacheries.underSiege, Enemies.championOfRhun]
      `shouldBe` [1, 1, 1, 2, 2]

  it "stages Thane Ulchor and Gate of Dorwinion, a location for each player, and makes a Makeshift Fortification active" . warTest $ \pid -> do
    flipFirstQuest pid
    chooseOptionMatching "choose a location" pid (const True)
    g <- getGame
    let staged = [toCardCode l | l <- toList g.entities.locations, l.placement == StagingArea]
    toCardCode Locations.gateOfDorwinion `elem` staged `shouldBe` True
    length staged `shouldBe` 2
    [toCardCode l | l <- toList g.entities.locations, l.placement == ActiveLocation] `shouldBe` [toCardCode Locations.makeshiftFortification]
    [(toCardCode e, e.placement) | e <- toList g.entities.enemies] `shouldBe` [(toCardCode Enemies.thaneUlchorWrathAndRuin, StagingArea)]
    deck <- map toCardCode . (.encounterDeck) <$> scenarioAttrs
    any (`elem` map toCardCode [Enemies.thaneUlchorWrathAndRuin, Locations.gateOfDorwinion]) deck `shouldBe` False
    length (filter (== toCardCode Locations.makeshiftFortification) deck) `shouldBe` 2
    (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

  it "has each player add a location unlike those already staged" . warTable $ \one two -> do
    q <- putQuestStage Quests.insideTheWalls
    _ <-
      setEncounterDeck
        [Enemies.thaneUlchorWrathAndRuin, Locations.gateOfDorwinion, Locations.makeshiftFortification, Locations.ransackedHouse, Locations.ransackedHouse, Locations.capturedTower]
    run $ PerformSetup q
    house <- fromJustNote "no Ransacked House" . find ((== toCardCode Locations.ransackedHouse) . toCardCode) <$> getEncounterDeck
    chooseTarget one (CardIdTarget house.id)
    Just question <- getQuestion two
    let offered = [c | TargetLabel (CardIdTarget c) _ <- questionChoices question]
    deck <- getEncounterDeck
    [toCardCode c | c <- deck, c.id `elem` offered] `shouldBe` map toCardCode [Locations.makeshiftFortification, Locations.capturedTower]

  -- \* Inside the Walls

  it "gives the first player control of an explored location, without its progress" . warTest $ \pid -> do
    _ <- stageB Quests.insideTheWalls
    lid <- activeLocation Locations.ransackedHouse
    run $ PlaceTokens GameSource (toTarget lid) Progress 4
    placementOf lid `shouldReturn` InPlayArea pid
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "gets +4 quest points per player and keeps player cards from placing progress in the staging area" . warTest $ \_ -> do
    q <- stageB Quests.insideTheWalls
    getModifiers q >>= (`shouldSatisfy` elem (QuestPointModifier 4))
    lid <- stageLocation Locations.ransackedHouse
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens (HeroSource $ toId beravor) (toTarget lid) Progress 1
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "is defeated only at the end of the round, with more locations controlled than staged, revealing Drive them Out" . warTest $ \pid -> do
    q <- stageB Quests.insideTheWalls
    cards <- traverse genEncounterCard [Quests.driveThemOut]
    putGame . (scenarioL %~ overAttrs (\a -> a {scenarioQuestDeck = cards})) =<< getGame
    _ <- setEncounterDeck []
    staged <- stageLocation Locations.ransackedHouse
    run $ PlaceTokens GameSource q Progress 8
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.insideTheWalls
    _ <- controlled pid Locations.contestedSquare
    roundEnds
    -- one controlled, one staged: not more
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.insideTheWalls
    run $ PlaceLocation staged (InPlayArea pid)
    roundEnds
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.driveThemOut

  -- \* Drive them Out

  it "returns each player's highest threat location, then has each discard to an enemy engaged with him, as Drive them Out is revealed" . warTest $ \pid -> do
    house <- controlled pid Locations.ransackedHouse
    square <- controlled pid Locations.contestedSquare
    inDiscard [Enemies.championOfRhun]
    _ <- setEncounterDeck []
    q <- putQuestStage Quests.driveThemOut
    run $ Flip q
    resolveWhenRevealed pid
    placementOf house `shouldReturn` StagingArea
    placementOf square `shouldReturn` InPlayArea pid
    champion <- enemyOf Enemies.championOfRhun
    placementOfEnemy champion `shouldReturn` EngagedWith pid

  it "lets the players travel to Gate of Dorwinion only with 12 progress on stage 2B and more locations controlled than staged" . warTest $ \pid -> do
    q <- stageB Quests.driveThemOut
    gate <- stageLocation Locations.gateOfDorwinion
    travelableLocations `shouldReturn` []
    run $ PlaceTokens GameSource q Progress 12
    -- not defeated: there is no stage 3
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.driveThemOut
    travelableLocations `shouldReturn` []
    _ <- controlled pid Locations.ransackedHouse
    _ <- controlled pid Locations.contestedSquare
    travelableLocations `shouldReturn` [gate]

  it "is won at the end of the round once the players control Gate of Dorwinion" . warTest $ \pid -> do
    _ <- stageB Quests.driveThemOut
    gate <- stageLocation Locations.gateOfDorwinion
    roundEnds
    getsGame (.outcome) `shouldReturn` Nothing
    run $ PlaceLocation gate (InPlayArea pid)
    roundEnds
    getsGame (.outcome) `shouldReturn` Just Won

  it "gives control of an explored location on stage 2B too, Gate of Dorwinion in place of the victory display" . warTest $ \pid -> do
    _ <- stageB Quests.driveThemOut
    gate <- activeLocation Locations.gateOfDorwinion
    run $ PlaceTokens GameSource (toTarget gate) Progress 5
    placementOf gate `shouldReturn` InPlayArea pid

  -- \* Controlling locations

  it "returns an eliminated player's locations to the staging area, without their progress" . warTable $ \_ two -> do
    lid <- controlled two Locations.ransackedHouse
    run $ PlaceTokens GameSource (toTarget lid) Progress 1
    run $ EliminatePlayer two
    placementOf lid `shouldReturn` StagingArea
    (.progress) <$> getLocation lid `shouldReturn` 0

  -- \* Thane Ulchor

  it "keeps Thane Ulchor in the staging area, immune to player card effects" . warTest $ \pid -> do
    thane <- stage Enemies.thaneUlchorWrathAndRuin
    run $ EngageEnemy pid thane
    placementOfEnemy thane `shouldReturn` StagingArea
    getModifiers thane >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  it "has the first player discard to a location for the staging area after the active location is explored" . warTest $ \pid -> do
    _ <- stage Enemies.thaneUlchorWrathAndRuin
    lid <- activeLocation Locations.ransackedHouse
    _ <- setEncounterDeck [Enemies.championOfRhun, Locations.capturedTower, Locations.contestedSquare]
    run $ PlaceTokens GameSource (toTarget lid) Progress 4
    chooseLabel "$label.thaneUlchorWrathAndRuin.location" pid
    tower <- locationOf Locations.capturedTower
    placementOf tower `shouldReturn` StagingArea
    -- above the explored Ransacked House, controlled by no one without the stage
    discardCodes `shouldReturn` map toCardCode [Enemies.championOfRhun, Locations.ransackedHouse]
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.contestedSquare]

  it "has Thane Ulchor attack the first player, if he chooses, after the active location is explored" . warTest $ \pid -> do
    thane <- stage Enemies.thaneUlchorWrathAndRuin
    lid <- activeLocation Locations.ransackedHouse
    _ <- setEncounterDeck []
    run $ PlaceTokens GameSource (toTarget lid) Progress 4
    chooseLabel "$label.thaneUlchorWrathAndRuin.attack" pid
    passActions pid
    questionSource pid `shouldReturn` Just (toTarget thane)

  -- \* Champion of Rhûn

  it "has the player return a location he controls, or be attacked, as Champion of Rhûn engages him" . warTest $ \pid -> do
    house <- controlled pid Locations.ransackedHouse
    champion <- engaged pid Enemies.championOfRhun
    chooseTarget pid house
    placementOf house `shouldReturn` StagingArea
    run $ PlaceEnemy champion StagingArea
    run $ EngageEnemy pid champion
    -- with no location to return, he attacks
    passActions pid
    questionSource pid `shouldReturn` Just (toTarget champion)

  it "deals 1 damage to the defender with Champion of Rhûn's shadow" . warTest $ \pid -> do
    raider <- engaged pid Enemies.easterlingRaider
    beravor <- hero "01012"
    shadowAttack pid raider Enemies.championOfRhun beravor
    -- Easterling Raider's 2 attack against Beravor's 2 defense, and the shadow's 1
    getDamage beravor `shouldReturn` 1

  -- \* Easterling Skirmisher

  it "has surge and archery 1, and returns the highest threat location of the controller of a character it destroys" . warTest $ \pid -> do
    traverse_ (\k -> elem k (cdKeywords Enemies.easterlingSkirmisher) `shouldBe` True) [Surge, Archery 1]
    forum <- controlled pid Locations.cityForum
    tower <- controlled pid Locations.capturedTower
    skirmisher <- engaged pid Enemies.easterlingSkirmisher
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ AttackDamage skirmisher guard'
    placementOf tower `shouldReturn` StagingArea
    placementOf forum `shouldReturn` InPlayArea pid

  -- \* Gate of Dorwinion

  it "has Gate of Dorwinion's threat be 1 more than the locations controlled, immune to player card effects" . warTest $ \pid -> do
    gate <- stageLocation Locations.gateOfDorwinion
    getLocationThreat gate `shouldReturn` 1
    _ <- controlled pid Locations.ransackedHouse
    getLocationThreat gate `shouldReturn` 2
    getModifiers gate >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  it "has Thane Ulchor attack each player as the players travel to Gate of Dorwinion" . warTest $ \pid -> do
    thane <- stage Enemies.thaneUlchorWrathAndRuin
    gate <- stageLocation Locations.gateOfDorwinion
    run $ TravelTo gate
    passActions pid
    questionSource pid `shouldReturn` Just (toTarget thane)

  -- \* Contested Square

  it "places progress on Contested Square in the staging area after an enemy is defeated, and returns it once its controller's character is destroyed" . warTest $ \pid -> do
    square <- stageLocation Locations.contestedSquare
    raider <- engaged pid Enemies.easterlingRaider
    run $ DealDamage GameSource (toTarget raider) 3
    (.progress) <$> getLocation square `shouldReturn` 1
    run $ PlaceLocation square (InPlayArea pid)
    raider' <- engaged pid Enemies.easterlingRaider
    run $ DealDamage GameSource (toTarget raider') 3
    -- none while it is under his control
    (.progress) <$> getLocation square `shouldReturn` 1
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource guard' 3
    placementOf square `shouldReturn` StagingArea

  -- \* Ransacked House

  it "puts the topmost enemy of the encounter discard pile into play engaged to take control of Ransacked House" . warTest $ \pid -> do
    house <- stageLocation Locations.ransackedHouse
    inDiscard [Treacheries.fury, Enemies.easterlingRaider, Enemies.championOfRhun]
    inPhase EncounterPhase
    run $ ActionWindow pid
    chooseAbilityOf pid house
    placementOf house `shouldReturn` InPlayArea pid
    raider <- enemyOf Enemies.easterlingRaider
    placementOfEnemy raider `shouldReturn` EngagedWith pid
    discardCodes `shouldReturn` map toCardCode [Treacheries.fury, Enemies.championOfRhun]

  it "resolves Champion of Rhûn's engagement before Ransacked House is taken: with no location, he attacks" . warTest $ \pid -> do
    house <- stageLocation Locations.ransackedHouse
    inDiscard [Enemies.championOfRhun]
    inPhase EncounterPhase
    run $ ActionWindow pid
    chooseAbilityOf pid house
    champion <- enemyOf Enemies.championOfRhun
    passActions pid
    questionSource pid `shouldReturn` Just (toTarget champion)

  it "offers Ransacked House's action only in the encounter phase with an enemy in the discard pile" . warTest $ \pid -> do
    house <- stageLocation Locations.ransackedHouse
    inDiscard [Treacheries.fury]
    inPhase EncounterPhase
    run $ ActionWindow pid
    offersAbilityOf pid house `shouldReturn` False

  it "returns Ransacked House once its controller loses control of another location" . warTest $ \pid -> do
    house <- controlled pid Locations.ransackedHouse
    square <- controlled pid Locations.contestedSquare
    run $ PlaceLocation square StagingArea
    placementOf house `shouldReturn` StagingArea

  -- \* Makeshift Fortification

  it "has archery 3 while active, and returns the topmost enemy of the discard pile to the staging area as a player takes control of it" . warTest $ \pid -> do
    fort <- activeLocation Locations.makeshiftFortification
    getArcheryTotal `shouldReturn` 3
    inDiscard [Treacheries.fury, Enemies.championOfRhun]
    run $ PlaceLocation fort (InPlayArea pid)
    getArcheryTotal `shouldReturn` 0
    champion <- enemyOf Enemies.championOfRhun
    placementOfEnemy champion `shouldReturn` StagingArea

  -- \* Captured Tower

  it "keeps encounter card effects from being canceled while Captured Tower is active" . warTest $ \_ -> do
    _ <- activeLocation Locations.capturedTower
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)

  it "shuffles the discard pile in and discards to an enemy engaged with the player who takes control of Captured Tower" . warTest $ \pid -> do
    tower <- stageLocation Locations.capturedTower
    _ <- setEncounterDeck []
    inDiscard [Enemies.championOfRhun]
    run $ PlaceLocation tower (InPlayArea pid)
    champion <- enemyOf Enemies.championOfRhun
    placementOfEnemy champion `shouldReturn` EngagedWith pid

  -- \* Fury

  it "attaches Fury to the lowest engagement cost Easterling, which it saves from destruction once, healed" . warTest $ \pid -> do
    raider <- stage Enemies.easterlingRaider
    _ <- stage Enemies.championOfRhun
    reveal [Treacheries.fury, Locations.ransackedHouse]
    attached <- getsGame (map (.placement) . toList . (.entities.treacheries))
    attached `shouldBe` [AttachedTo (EnemyTarget raider)]
    -- surge
    _ <- locationOf Locations.ransackedHouse
    getModifiers (EnemyTarget raider) >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)
    run $ EngageEnemy pid raider
    run $ DealDamage GameSource (toTarget raider) 3
    placementOfEnemy raider `shouldReturn` EngagedWith pid
    (.damage) <$> getEnemy raider `shouldReturn` 0
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    run $ DealDamage GameSource (toTarget raider) 3
    placementOfEnemy raider `shouldReturn` Unplaced

  -- \* Caught Off Guard

  it "has a player who controls no location discard to an enemy engaged with him with Caught Off Guard" . warTest $ \pid -> do
    reveal [Treacheries.caughtOffGuard, Locations.capturedTower, Enemies.easterlingRaider]
    raider <- enemyOf Enemies.easterlingRaider
    placementOfEnemy raider `shouldReturn` EngagedWith pid
    discardCodes `shouldReturn` map toCardCode [Treacheries.caughtOffGuard, Locations.capturedTower]

  it "lets a player who controls a location return it with Caught Off Guard instead" . warTest $ \pid -> do
    house <- controlled pid Locations.ransackedHouse
    reveal [Treacheries.caughtOffGuard, Enemies.easterlingRaider]
    chooseTarget pid house
    placementOf house `shouldReturn` StagingArea
    enemyCodes `shouldReturn` []

  it "has the attacking enemy attack again with Caught Off Guard's shadow" . warTest $ \pid -> do
    raider <- engaged pid Enemies.easterlingRaider
    shadowAttack pid raider Treacheries.caughtOffGuard =<< hero "01012"
    questionSource pid `shouldReturn` Just (toTarget raider)

  -- \* Street Fighting

  it "has each player assign 1 more damage than the locations he controls with Street Fighting" . warTest $ \pid -> do
    _ <- controlled pid Locations.ransackedHouse
    reveal [Treacheries.streetFightingWrathAndRuin]
    beravor <- hero "01012"
    chooseTarget pid beravor
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2

  it "has the defending player assign 1 more damage than the locations he controls with Street Fighting's shadow" . warTest $ \pid -> do
    raider <- engaged pid Enemies.easterlingRaider
    eowyn <- hero "01007"
    shadowAttack pid raider Treacheries.streetFightingWrathAndRuin eowyn
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1

  -- \* Under Siege

  it "adds 1 more threat than the locations controlled to the staging area this phase with Under Siege" . warTest $ \pid -> do
    _ <- controlled pid Locations.ransackedHouse
    threat <- getStagingThreat
    reveal [Treacheries.underSiege]
    getStagingThreat `shouldReturn` threat + 2

  it "gives the attacking enemy +1 attack with Under Siege's shadow" . warTest $ \pid -> do
    house <- controlled pid Locations.ransackedHouse
    raider <- engaged pid Enemies.easterlingRaider
    beravor <- hero "01012"
    shadowAttack pid raider Treacheries.underSiege beravor
    -- Easterling Raider's 2 attack and the shadow's 1 against Beravor's 2 defense
    getDamage beravor `shouldReturn` 1
    placementOf house `shouldReturn` InPlayArea pid

  it "returns the defender's highest threat location if the attack destroys a character with Under Siege's shadow" . warTest $ \pid -> do
    forum <- controlled pid Locations.cityForum
    tower <- controlled pid Locations.capturedTower
    raider <- engaged pid Enemies.easterlingRaider
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    shadowAttack pid raider Treacheries.underSiege guard'
    placementOf tower `shouldReturn` StagingArea
    placementOf forum `shouldReturn` InPlayArea pid

  -- \* Ulchor's Revenge

  it "has Thane Ulchor attack only the players who control a location with Ulchor's Revenge" . warTest $ \pid -> do
    thane <- stage Enemies.thaneUlchorWrathAndRuin
    reveal [Treacheries.ulchorsRevenge]
    passActions pid
    questionSource pid `shouldReturn` Nothing
    _ <- controlled pid Locations.ransackedHouse
    reveal [Treacheries.ulchorsRevenge]
    passActions pid
    questionSource pid `shouldReturn` Just (toTarget thane)

  it "gives the attacking enemy +1 attack for each location the defender controls with Ulchor's Revenge's shadow" . warTest $ \pid -> do
    _ <- controlled pid Locations.ransackedHouse
    _ <- controlled pid Locations.contestedSquare
    raider <- engaged pid Enemies.easterlingRaider
    beravor <- hero "01012"
    shadowAttack pid raider Treacheries.ulchorsRevenge beravor
    -- Easterling Raider's 2 attack and 2 for the locations against Beravor's 2 defense
    getDamage beravor `shouldReturn` 2
