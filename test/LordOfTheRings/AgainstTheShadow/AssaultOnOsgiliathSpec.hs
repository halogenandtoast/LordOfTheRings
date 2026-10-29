module LordOfTheRings.AgainstTheShadow.AssaultOnOsgiliathSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (currentQuestHas, getArcheryTotal)
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (assaultOnOsgiliath, lookupScenario)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

osgiliathTest :: (PlayerId -> GameT ()) -> IO ()
osgiliathTest = gameTestIn assaultOnOsgiliath testDeck

-- | A table of two players, both with the test deck.
osgiliathTable :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
osgiliathTable body = do
  seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", testDeck)])
  env <- newGameEnv (newTableIn assaultOnOsgiliath seats) (const $ pure ()) 1
  runGameT env do
    order <- getsGame (.playerOrder)
    case order of
      [one, two] -> body one two
      _ -> error "two players"

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

-- | Put the location into play in the staging area.
stage :: CardDef -> GameT LocationId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

-- | Put the location into play under the player's control.
controlled :: PlayerId -> CardDef -> GameT LocationId
controlled pid def = do
  lid <- stage def
  run $ PlaceLocation lid (InPlayArea pid)
  pure lid

-- | Retake the City in play, flipped, with an empty encounter deck to search.
retake :: GameT Target
retake = do
  quest <- putQuestStage Quests.retakeTheCity
  _ <- setEncounterDeck []
  run $ Flip quest
  pure quest

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  run . AddToStaging =<< genEncounterCard def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "Assault on Osgiliath" do
  it "is a scenario the game can be dealt" . osgiliathTest $ \_ ->
    isJust (lookupScenario "assault-on-osgiliath") `shouldBe` True

  it "has each player add an enemy and a unique location to the staging area at setup" . osgiliathTest $ \pid -> do
    flipFirstQuest pid
    -- the enemy, then the unique location, each chosen from those in the deck
    chooseOptionMatching "choose an enemy" pid (const True)
    chooseOptionMatching "choose a unique location" pid (const True)
    g <- getGame
    length [e | e <- toList g.entities.enemies, e.placement == StagingArea] `shouldBe` 1
    map toCardCode [l | l <- toList g.entities.locations, l.placement == StagingArea]
      `shouldSatisfy` all (`elem` map toCardCode [Locations.westGate, Locations.theKingsLibrary, Locations.ancientHarbor, Locations.theOldBridge])
    length (toList g.entities.locations) `shouldBe` 1

  it "gives the first player control of an explored Osgiliath location, without its progress" . osgiliathTest $ \pid -> do
    _ <- retake
    lid <- stage Locations.westQuarter
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceTokens GameSource (toTarget lid) Progress 3
    placementOf Locations.westQuarter `shouldReturn` Just (InPlayArea pid)
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "keeps player card effects from placing progress on locations in the staging area" . osgiliathTest $ \pid -> do
    _ <- retake
    lid <- stage Locations.westQuarter
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens (HeroSource $ toId beravor) (toTarget lid) Progress 1
    (.progress) <$> getLocation lid `shouldReturn` 0
    run $ PlaceLocation lid (InPlayArea pid)
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceTokens (HeroSource $ toId beravor) (toTarget lid) Progress 1
    (.progress) <$> getLocation lid `shouldReturn` 1

  it "is won at the end of the round once the players control every Osgiliath location" . osgiliathTest $ \pid -> do
    _ <- retake
    _ <- controlled pid Locations.westQuarter
    lid <- stage Locations.eastQuarter
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Nothing
    run $ PlaceLocation lid (InPlayArea pid)
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Won

  it "returns an eliminated player's locations to the staging area" . osgiliathTable $ \_ two -> do
    _ <- retake
    _ <- controlled two Locations.westQuarter
    run $ EliminatePlayer two
    placementOf Locations.westQuarter `shouldReturn` Just StagingArea

  it "returns West Gate to the staging area once its controller lets an attack go undefended" . osgiliathTest $ \pid -> do
    _ <- controlled pid Locations.westGate
    eid <- engaged pid Enemies.southronPhalanx
    run $ UndefendedAttack pid eid
    placementOf Locations.westGate `shouldReturn` Just StagingArea

  it "makes a second active location of Ruined Tower, and the first player divides the progress" . osgiliathTest $ \pid -> do
    quarter <- stage Locations.eastQuarter
    run $ PlaceLocation quarter ActiveLocation
    tower <- stage Locations.ruinedTower
    _ <- stage Locations.westQuarter
    travelableLocations `shouldReturn` [tower]
    run $ TravelTo tower
    chooseTarget pid =<< heroOf pid "01012"
    placementOf Locations.ruinedTower `shouldReturn` Just ActiveLocation
    travelableLocations `shouldReturn` []
    run $ PlaceProgressOnQuest 3
    -- 1 to 3 may go on East Quarter, the rest on Ruined Tower: the second choice is 2
    void $ answerQuestion pid 1
    (.progress) <$> getLocation quarter `shouldReturn` 2
    (.progress) <$> getLocation tower `shouldReturn` 1

  it "passes progress on to the quest only once both active locations are explored" . osgiliathTest $ \_ -> do
    _ <- putQuestStage Quests.retakeTheCity
    quarter <- stage Locations.eastQuarter
    tower <- stage Locations.ruinedTower
    run $ PlaceLocation quarter ActiveLocation
    run $ PlaceLocation tower ActiveLocation
    run $ PlaceProgressOnQuest 6
    getsGame (any ((`elem` [quarter, tower]) . toId) . toList . (.entities.locations)) `shouldReturn` False
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 1

  it "returns Ruined Tower once its controller loses control of another location" . osgiliathTest $ \pid -> do
    _ <- controlled pid Locations.ruinedTower
    quarter <- controlled pid Locations.westQuarter
    run $ PlaceLocation quarter StagingArea
    placementOf Locations.ruinedTower `shouldReturn` Just StagingArea

  it "returns Ruined Tower once a location its controller controls is discarded" . osgiliathTest $ \pid -> do
    _ <- controlled pid Locations.ruinedTower
    quarter <- controlled pid Locations.westQuarter
    run $ Discard (toTarget quarter)
    placementOf Locations.ruinedTower `shouldReturn` Just StagingArea

  it "gives the current quest battle or siege while East or West Quarter is active" . osgiliathTest $ \_ -> do
    _ <- retake
    east <- stage Locations.eastQuarter
    run $ PlaceLocation east ActiveLocation
    currentQuestHas Battle `shouldReturn` True
    run $ PlaceLocation east StagingArea
    west <- stage Locations.westQuarter
    run $ PlaceLocation west ActiveLocation
    currentQuestHas Siege `shouldReturn` True
    currentQuestHas Battle `shouldReturn` False

  it "places progress on Ruined Square after an enemy is defeated, and returns it once a character leaves play" . osgiliathTest $ \pid -> do
    square <- stage Locations.ruinedSquare
    eid <- engaged pid Enemies.southronPhalanx
    run $ DealDamage GameSource (toTarget eid) 3
    chooseAbilityOf pid square
    (.progress) <$> getLocation square `shouldReturn` 1
    run $ PlaceLocation square (InPlayArea pid)
    scout <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ Discard scout
    placementOf Locations.ruinedSquare `shouldReturn` Just StagingArea

  it "places progress on Ruined Square under a player's control too (ruling)" . osgiliathTest $ \pid -> do
    square <- controlled pid Locations.ruinedSquare
    eid <- engaged pid Enemies.southronPhalanx
    run $ DealDamage GameSource (toTarget eid) 3
    chooseAbilityOf pid square
    (.progress) <$> getLocation square `shouldReturn` 1

  it "has each player return his highest threat location or raise his threat with Counter-attack" . osgiliathTest $ \pid -> do
    _ <- controlled pid Locations.westQuarter
    _ <- controlled pid Locations.ruinedSquare
    threat <- threatOf pid
    reveal [Treacheries.counterAttack, Treacheries.counterAttack, Locations.ruinedTower]
    chooseOptionMatching "raise threat" pid \case Label l _ -> "$label.counterAttack.raise" `isPrefixOf` l; _ -> False
    threatOf pid `shouldReturn` threat + 3
    -- its surge reveals the second
    resolveRevealed
    chooseLabel "$label.counterAttack.return" pid
    placementOf Locations.westQuarter `shouldReturn` Just StagingArea
    placementOf Locations.ruinedSquare `shouldReturn` Just (InPlayArea pid)

  it "adds the locations the players control to the archery total with Pinned Down, surging under 4" . osgiliathTest $ \pid -> do
    _ <- controlled pid Locations.westQuarter
    _ <- controlled pid Locations.eastQuarter
    reveal [Treacheries.pinnedDown, Locations.ruinedTower]
    getArcheryTotal `shouldReturn` 2
    placementOf Locations.ruinedTower `shouldReturn` Just StagingArea

  it "gives Southron Phalanx +1 attack for each location its player controls" . osgiliathTest $ \pid -> do
    eid <- engaged pid Enemies.southronPhalanx
    getEnemyAttack eid `shouldReturn` 1
    _ <- controlled pid Locations.westQuarter
    _ <- controlled pid Locations.eastQuarter
    getEnemyAttack eid `shouldReturn` 3

  it "attacks each player who controls a location as Southron Commander is revealed" . osgiliathTest $ \pid -> do
    reveal [Enemies.southronCommander]
    q <- getQuestion pid
    q `shouldBe` Nothing
    _ <- controlled pid Locations.westQuarter
    reveal [Enemies.southronCommander]
    passActions pid
    asked <- getQuestion pid
    [l | Just (QuestionLabel l _ _ _) <- [asked]] `shouldSatisfy` any ("$label.declareDefender" `isPrefixOf`)

  it "has the player return a location once Uruk Soldier's attack destroys a character" . osgiliathTest $ \pid -> do
    _ <- controlled pid Locations.westQuarter
    eid <- engaged pid Enemies.urukSoldier
    scout <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ AttackDamage eid scout
    placementOf Locations.westQuarter `shouldReturn` Just StagingArea

  it "has the player return his highest threat location when East Quarter's shadow attack destroys a character" . osgiliathTest $ \pid -> do
    _ <- retake
    _ <- controlled pid Locations.ancientHarbor
    _ <- controlled pid Locations.westQuarter
    eid <- engaged pid Enemies.southronPhalanx
    _ <- setEncounterDeck [Locations.eastQuarter]
    run $ DealShadowCard eid
    scout <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid scout
    passActions pid
    placementOf Locations.westQuarter `shouldReturn` Just StagingArea
    placementOf Locations.ancientHarbor `shouldReturn` Just (InPlayArea pid)

  it "discards to an Osgiliath location the first player may take control of with Street Fighting" . osgiliathTest $ \pid -> do
    reveal [Treacheries.streetFighting, Enemies.urukSoldier, Locations.westQuarter, Locations.ruinedTower]
    beravor <- heroOf pid "01012"
    chooseTarget pid beravor
    placementOf Locations.westQuarter `shouldReturn` Just (InPlayArea pid)
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Enemies.urukSoldier))
    placementOf Locations.ruinedTower `shouldReturn` Nothing

  it "spends 2 resources, or 1 beside another Osgiliath location, for progress on The Old Bridge" . osgiliathTest $ \pid -> do
    bridge <- stage Locations.theOldBridge
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 3
    _ <- stage Locations.westQuarter
    run $ BeginPhase PlanningPhase
    chooseAbilityOf pid bridge
    (.progress) <$> getLocation bridge `shouldReturn` 1
    getHeroResources (toId beravor) `shouldReturn` 2

  it "adds an Osgiliath location from the encounter deck and becomes active with West Gate's action" . osgiliathTest $ \pid -> do
    gate <- stage Locations.westGate
    _ <- setEncounterDeck [Enemies.urukSoldier, Locations.eastQuarter]
    run $ ActionWindow pid
    chooseAbilityOf pid gate
    chooseTarget pid . fromJustNote "no East Quarter" . find ((== toCardCode Locations.eastQuarter) . toCardCode) =<< getEncounterDeck
    resolveRevealed
    placementOf Locations.eastQuarter `shouldReturn` Just StagingArea
    placementOf Locations.westGate `shouldReturn` Just ActiveLocation

  it "adds the topmost Orc enemy in the encounter discard pile to the staging area with Uruk Lieutenant" . osgiliathTest $ \_ -> do
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.urukSoldier
    reveal [Enemies.urukLieutenant]
    placementOf Enemies.urukSoldier `shouldReturn` Just StagingArea
