module LordOfTheRings.RingMaker.TroubleInTharbadSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (EliminationThreatModifier, EngagementCostModifier, ThreatModifier))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, troubleInTharbad)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PhaseBegins, ThreatReduced))
import TestImport

tharbadTest :: (PlayerId -> GameT ()) -> IO ()
tharbadTest = gameTestIn troubleInTharbad testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [a.placement | a <- toList g.entities.allies, toCardCode a == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

nalir :: GameT AllyId
nalir = getsGame (fromJustNote "no Nalir" . fmap toId . find ((== toCardCode Objectives.nalirTroubleInTharbad) . toCardCode) . toList . (.entities.allies))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

questTimeCounters :: GameT Int
questTimeCounters = getTimeCounters . toTarget =<< getActiveQuest

-- | Setup done, The Empty Mug explored into the victory display.
withMugExplored :: PlayerId -> GameT ()
withMugExplored pid = do
  flipFirstQuest pid
  mug <- locationOf Locations.theEmptyMug
  run $ PlaceTokens GameSource (toTarget mug) Progress 4

-- | On to Escape from Tharbad, its When Revealed resolved.
toEscape :: PlayerId -> GameT ()
toEscape pid = do
  withMugExplored pid
  _ <- setEncounterDeck [Enemies.spyFromMordor]
  run . CompleteQuest . toId =<< getActiveQuest
  resolveWhenRevealed pid

spec :: Spec
spec = describe "Trouble in Tharbad" do
  it "is a scenario the game can be dealt" . tharbadTest $ \_ ->
    isJust (lookupScenario "trouble-in-tharbad") `shouldBe` True

  it "stages a Spy from Mordor, makes The Empty Mug active, gives Nalir to the first player and sets the others aside" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    placementOf Enemies.spyFromMordor `shouldReturn` Just StagingArea
    placementOf Locations.theEmptyMug `shouldReturn` Just ActiveLocation
    placementOf Objectives.nalirTroubleInTharbad `shouldReturn` Just (InPlayArea pid)
    deck <- map toCardCode <$> getEncounterDeck
    for_ [Enemies.bellachTroubleInTharbad, Locations.theCrossingAtTharbad, Locations.theEmptyMug] \c ->
      deck `shouldSatisfy` notElem (toCardCode c)
    questTimeCounters `shouldReturn` 4

  it "keeps the players' threat from going down while The Empty Mug is in play" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    threat <- threatOf pid
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 3
    threatOf pid `shouldReturn` threat

  it "turns progress on Double Dealings into lower threat, and advances once every threat is 0" . tharbadTest $ \pid -> do
    withMugExplored pid
    threat <- threatOf pid
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 5
    threatOf pid `shouldReturn` threat - 5
    (.progress) <$> getActiveQuest `shouldReturn` 0
    run $ PlaceTokens GameSource q Progress (threat - 5)
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.escapeFromTharbad

  it "lowers the threat elimination level by 10 as Double Dealings loses its last time counter" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    q <- toTarget <$> getActiveQuest
    run $ RemoveTokens GameSource q TimeCounter 4
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (EliminationThreatModifier (-10)))
    questTimeCounters `shouldReturn` 4
    threat <- threatOf pid
    run $ RaiseThreat pid (40 - threat)
    run CheckEliminations
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "removes a player card that reduces threat from the game once The Empty Mug is in the victory display" . tharbadTest $ \pid -> do
    withMugExplored pid
    beravor <- getHeroWithCode "01012"
    run $ CheckWindows [Window After (ThreatReduced pid (HeroSource $ toId beravor))]
    heroes <- getsGame (map toCardCode . toList . (.entities.heroes))
    heroes `shouldSatisfy` notElem "01012"
    discard <- (.discard) <$> getPlayer pid
    map toCardCode discard `shouldSatisfy` notElem "01012"

  it "has Spy from Mordor remove a time counter as it attacks" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    spy <- enemyOf Enemies.spyFromMordor
    run $ EngageEnemy pid spy
    _ <- setEncounterDeck [Treacheries.constantTail]
    run $ EnemyAttack pid spy
    questTimeCounters `shouldReturn` 3

  it "keeps time counters on the quest while Tharbad Hideout is active" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    mug <- locationOf Locations.theEmptyMug
    run $ Discard (toTarget mug)
    hideout <- stagedLocation Locations.tharbadHideout
    run $ TravelTo hideout
    q <- toTarget <$> getActiveQuest
    run $ RemoveTokens GameSource q TimeCounter 1
    questTimeCounters `shouldReturn` 4

  it "explores Tharbad Hideout in the staging area as the quest phase begins" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    _ <- stagedLocation Locations.tharbadHideout
    run $ CheckWindows [Window After (PhaseBegins QuestPhase)]
    placementOf Locations.tharbadHideout `shouldReturn` Nothing

  it "raises threat by the enemies in play to travel to Hidden Alleyway, then adds a time counter" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    mug <- locationOf Locations.theEmptyMug
    run $ Discard (toTarget mug)
    alley <- stagedLocation Locations.hiddenAlleyway
    threat <- threatOf pid
    run $ TravelTo alley
    threatOf pid `shouldReturn` threat + 1
    questTimeCounters `shouldReturn` 5

  it "keeps progress off Streets of Tharbad in the staging area" . tharbadTest $ \_ -> do
    streets <- stagedLocation Locations.streetsOfTharbad
    run $ PlaceTokens GameSource (toTarget streets) Progress 1
    placementOf Locations.streetsOfTharbad `shouldReturn` Just StagingArea

  it "gives City locations +1 threat while Ruins of the Second Age is in the staging area" . tharbadTest $ \_ -> do
    streets <- stagedLocation Locations.streetsOfTharbad
    _ <- stagedLocation Locations.ruinsOfTheSecondAge
    getLocationThreat streets `shouldReturn` 4

  it "has Get That Dwarf! send the strongest staged enemy at Nalir, its undefended damage on him" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    _ <- staged Enemies.bellachsMarauder
    reveal [Treacheries.getThatDwarf]
    _ <- setEncounterDeck [Treacheries.constantTail]
    placementOf Enemies.bellachsMarauder `shouldReturn` Just (EngagedWith pid)
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    outcome `shouldReturn` Just Lost

  it "surges Conspicuous Lot while a player's threat is 20 or less" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    threat <- threatOf pid
    run $ LowerThreat pid (threat - 10)
    reveal [Treacheries.conspicuousLot, Locations.seedyInn]
    -- Nalir is an ally the player controls
    threatOf pid `shouldReturn` 11
    placementOf Locations.seedyInn `shouldReturn` Just StagingArea

  it "raises the defending player's threat by the damage dealt with Seedy Inn's shadow" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    spy <- enemyOf Enemies.spyFromMordor
    run $ EngageEnemy pid spy
    _ <- setEncounterDeck [Locations.seedyInn]
    run $ DealShadowCard spy
    threat <- threatOf pid
    run $ EnemyAttack pid spy
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01012"
    passActions pid
    -- Spy from Mordor's 3 attack, undefended
    threatOf pid `shouldReturn` threat + 3

  it "gives Bellach a threat per player and Orc enemies -30 engagement cost and +1 threat" . tharbadTest $ \_ -> do
    bellach <- staged Enemies.bellachTroubleInTharbad
    spy <- staged Enemies.spyFromMordor
    getModifiers (EnemyTarget spy) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [EngagementCostModifier (-30), ThreatModifier 1])
    getModifiers (EnemyTarget bellach) >>= (`shouldSatisfy` elem (ThreatModifier 1))

  it "stages Bellach, The Crossing at Tharbad and an Orc for each player as Escape from Tharbad is revealed" . tharbadTest $ \pid -> do
    toEscape pid
    placementOf Enemies.bellachTroubleInTharbad `shouldReturn` Just StagingArea
    placementOf Locations.theCrossingAtTharbad `shouldReturn` Just StagingArea
    spies <- getsGame (filter ((== toCardCode Enemies.spyFromMordor) . toCardCode) . toList . (.entities.enemies))
    length spies `shouldBe` 2

  it "places the stage's progress on The Crossing at Tharbad, won as it is explored" . tharbadTest $ \pid -> do
    toEscape pid
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 11
    crossing <- locationOf Locations.theCrossingAtTharbad
    (.progress) <$> getLocation crossing `shouldReturn` 11
    outcome `shouldReturn` Nothing
    run $ PlaceTokens GameSource q Progress 1
    outcome `shouldReturn` Just Won

  it "raises a character's controller's threat by 2 as it leaves play during Escape from Tharbad" . tharbadTest $ \pid -> do
    toEscape pid
    threat <- threatOf pid
    eowyn <- getHeroWithCode "01007"
    run $ Discard (toTarget eowyn)
    threatOf pid `shouldReturn` threat + 2

  it "loses the game as Nalir leaves play" . tharbadTest $ \pid -> do
    flipFirstQuest pid
    n <- nalir
    run $ Discard (toTarget n)
    outcome `shouldReturn` Just Lost
