module LordOfTheRings.TheLordOfTheRings.FlightToTheFordNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing, theOneRingExhausted)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost, getLocationThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (flightToTheFord)
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (getRingBearersLife)
import LordOfTheRings.Scenario.Types (
  GameMode (NightmareMode),
  burdenDeckL,
  modeL,
  scenarioSetAside,
 )
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) flightToTheFord

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

tableNightmare :: ([PlayerId] -> GameT ()) -> IO ()
tableNightmare body = do
  seats <-
    traverse
      (\i -> either (error . show) id <$> loadPlayer ("player" <> tshow i) testDeck)
      (1 :| [2 :: Int])
  env <- newGameEnv (newTableIn nightmareScenario seats) (const $ pure ()) 1
  runGameT env $ getsGame (.playerOrder) >>= body

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  beforeIds <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame
    (toId . fromJustNote "enemy" . find ((`notElem` beforeIds) . toId) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  beforeIds <- getsGame (map toId . toList . (.entities.locations))
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "location"
        . find ((`notElem` beforeIds) . toId)
        . toList
        . (.entities.locations)
    )

withRing :: PlayerId -> GameT Target
withRing _ = do
  run $ DoStep 3 Setup
  lead <- getLead
  getQuestion lead >>= \case
    Just _ -> void $ answerQuestion lead 0
    Nothing -> pure ()
  toTarget . fromJustNote "Ring" <$> getSagaOneRing

setLife :: Int -> GameT ()
setLife n = run $ ScenarioSpecific "flightToTheFord.setLife" (toJSON n)

passTableActions :: GameT ()
passTableActions = do
  questions <- getsGame (mapToList . (.question))
  case [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.takeAction"] of
    pid : _ -> passActions pid >> passTableActions
    [] -> pure ()

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

spec :: Spec
spec = describe "Flight to the Ford Nightmare" do
  it "builds all eight Nightmare card types and removes the prescribed standard cards" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map
      copies
      [ Enemies.fellRiderNightmare
      , Locations.trollshawForest
      , Locations.greatEastRoad
      , Locations.pathlessWilderness
      , Locations.ettendales
      , Treacheries.comeBack
      , Treacheries.theTwilightWorld
      ]
      `shouldBe` [4, 3, 3, 2, 2, 2, 2]
    map
      copies
      [ Enemies.fellRider
      , Locations.fordOfBruinen
      , Locations.theTrollsCamp
      , Locations.ettenmoors
      , Treacheries.painAssailedHim
      , Locations.theOldRoad
      , Locations.pathlessCountryHunted
      , Treacheries.strickenDumb
      , Treacheries.rodeLikeAGale
      ]
      `shouldBe` replicate 9 0
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    sort (map toCardCode aside)
      `shouldBe` sort (map toCardCode [Locations.fordOfBruinenNightmare, Enemies.theWitchKing])

  it "starts with one Nightmare Fell Rider per player" . tableNightmare $ \pids -> do
    run $ DoStep 1 Setup
    _ <- withRing (fromJustNote "first" $ headMay pids)
    q <- getActiveQuest
    run $ PerformSetup (toTarget q)
    getsGame (map toCardCode . toList . (.entities.enemies))
      `shouldReturn` replicate 2 (toCardCode Enemies.fellRiderNightmare)
    getRingBearersLife `shouldReturn` 15

  it "adds the Nightmare Ford at stage two" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    old <- getActiveQuest
    run $ LeaveQuestStage (toId old)
    _ <- putQuestStage Quests.raceToRivendell
    q <- getActiveQuest
    run $ Flip (toTarget q)
    getsGame (map toCardCode . toList . (.entities.locations))
      `shouldReturn` [toCardCode Locations.fordOfBruinenNightmare]

  it "keeps enough quest progress until the Ford is explored, then wins without extra progress" . nightmare $ \_ -> do
    ford <- stageLocation Locations.fordOfBruinenNightmare
    _ <- putQuestStage Quests.raceToRivendell
    q <- getActiveQuest
    run $ Flip (toTarget q)
    run $ PlaceTokens GameSource (toTarget q) Progress 40
    getsGame (.outcome) `shouldReturn` Nothing
    (.progress) <$> getActiveQuest `shouldReturn` 40
    run $ PlaceTokens GameSource (LocationTarget ford) Progress 3
    getsGame (.outcome) `shouldReturn` Just Won

  it "blocks direct victory while the Ford is in play" . nightmare $ \_ -> do
    _ <- stageLocation Locations.fordOfBruinenNightmare
    run $ GameOver Won
    getsGame (.outcome) `shouldReturn` Nothing

  it "does not add the Nightmare victory restriction to the standard scenario" . gameTestIn flightToTheFord testDeck $ \_ -> do
    _ <- stageLocation Locations.fordOfBruinenNightmare
    run $ GameOver Won
    getsGame (.outcome) `shouldReturn` Just Won

  it "has each player find and engage a Nazgul when traveling to the Ford" . tableNightmare $ \pids -> do
    ford <- stageLocation Locations.fordOfBruinenNightmare
    _ <- setEncounterDeck [Enemies.fellRiderNightmare, Enemies.ringwraith]
    run $ TravelTo ford
    for_ pids $ \pid -> chooseOptionMatching "find a Nazgul" pid (const True)
    placements <- getsGame (map (.placement) . toList . (.entities.enemies))
    for_ pids $ \pid -> length (filter (== EngagedWith pid) placements) `shouldBe` 1
    (.placement) <$> getLocation ford `shouldReturn` ActiveLocation

  it "does not discard engaged enemies when the Nightmare Ford is explored" . nightmare $ \pid -> do
    ford <- stageLocation Locations.fordOfBruinenNightmare
    enemy <- stageEnemy Enemies.fellRiderNightmare
    run $ EngageEnemy pid enemy
    run $ PlaceTokens GameSource (LocationTarget ford) Progress 3
    getsGame (length . (.entities.enemies)) `shouldReturn` 1

  it "has Fell Rider reduce life after destroying a defender" . nightmare $ \pid -> do
    setLife 15
    eid <- stageEnemy Enemies.fellRiderNightmare
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getRingBearersLife `shouldReturn` 14

  it "does not reduce life when Fell Rider's defender survives" . nightmare $ \pid -> do
    setLife 15
    eid <- stageEnemy Enemies.fellRiderNightmare
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate defender
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getRingBearersLife `shouldReturn` 15

  it "gives Fell Rider's shadow +1 attack while the Ring is ready" . nightmare $ \pid -> do
    _ <- withRing pid
    eid <- stageEnemy Enemies.goblinSniper
    base <- getEnemyAttack eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.fellRiderNightmare
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` (base + 1)

  it "gives Fell Rider's shadow +2 attack while the Ring is exhausted" . nightmare $ \pid -> do
    ring <- withRing pid
    run $ Exhaust ring
    eid <- stageEnemy Enemies.goblinSniper
    base <- getEnemyAttack eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.fellRiderNightmare
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` (base + 2)

  it "has staged Trollshaw Forest penalize cancellation with A Test of Will" . nightmare $ \pid -> do
    setLife 15
    _ <- stageLocation Locations.trollshawForest
    hero <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.aTestOfWill
    _ <- setEncounterDeck [Treacheries.comeBack]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getRingBearersLife `shouldReturn` 14

  it "does not penalize cancellation while Trollshaw Forest is active" . nightmare $ \pid -> do
    setLife 15
    lid <- stageLocation Locations.trollshawForest
    run $ PlaceLocation lid ActiveLocation
    hero <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.aTestOfWill
    _ <- setEncounterDeck [Treacheries.comeBack]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getRingBearersLife `shouldReturn` 15

  it "reduces life as Trollshaw Forest's travel cost" . nightmare $ \_ -> do
    setLife 15
    lid <- stageLocation Locations.trollshawForest
    run $ TravelTo lid
    getRingBearersLife `shouldReturn` 14
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "sets Great East Road's threat from all Nazgul, including engaged enemies" . nightmare $ \pid -> do
    lid <- stageLocation Locations.greatEastRoad
    getLocationThreat lid `shouldReturn` 1
    _ <- stageEnemy Enemies.fellRiderNightmare
    other <- stageEnemy Enemies.ringwraith
    run $ EngageEnemy pid other
    _ <- stageEnemy Enemies.goblinSniper
    getLocationThreat lid `shouldReturn` 3
    getEngagementCost other `shouldReturn` 30
    run $ PlaceLocation lid ActiveLocation
    getEngagementCost other `shouldReturn` 35

  it "stacks Great East Road's engagement-cost reduction" . nightmare $ \_ -> do
    _ <- stageLocation Locations.greatEastRoad
    _ <- stageLocation Locations.greatEastRoad
    eid <- stageEnemy Enemies.fellRiderNightmare
    getEngagementCost eid `shouldReturn` 20

  it "starts Pathless Wilderness with three resources and reduces life every round after it empties" . nightmare $ \_ -> do
    setLife 15
    lid <- stageLocation Locations.pathlessWilderness
    countTokens Resource . locationTokens . toAttrs <$> getLocation lid `shouldReturn` 3
    for_ [15, 15, 12, 9] $ \expected -> do
      run $ CheckWindows [mkAfter RoundEnds]
      getRingBearersLife `shouldReturn` expected

  it "loses when Pathless Wilderness empties the Ring-bearer's remaining life" . nightmare $ \_ -> do
    setLife 3
    _ <- stageLocation Locations.pathlessWilderness
    replicateM_ 3 $ run $ CheckWindows [mkAfter RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Lost

  it "shuffles the top burden into the encounter deck when Ettendales is explored" . nightmare $ \_ -> do
    burdens <- traverse genEncounterCard [Treacheries.weightOfTheRing, Treacheries.eatenAlive]
    putGame . (scenarioL %~ overAttrs (burdenDeckL .~ burdens)) =<< getGame
    lid <- stageLocation Locations.ettendales
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Treacheries.weightOfTheRing]
    getsGame (map toCardCode . (.burdenDeck) . toAttrs . (.scenario))
      `shouldReturn` [toCardCode Treacheries.eatenAlive]

  it "adds Ettendales' shadow attack bonus and life penalty after a kill" . nightmare $ \pid -> do
    setLife 15
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . AttachShadowCard eid =<< genEncounterCard Locations.ettendales
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getRingBearersLife `shouldReturn` 14

  it "has Come Back reduce life and remove all quest progress" . nightmare $ \pid -> do
    setLife 15
    _ <- putQuestStage Quests.raceToRivendell
    q <- getActiveQuest
    run $ PlaceTokens GameSource (toTarget q) Progress 4
    reveal [Treacheries.comeBack]
    getRingBearersLife `shouldReturn` 14
    (.progress) <$> getActiveQuest `shouldReturn` 0
    (.threat) <$> getPlayer pid `shouldReturn` 1

  it "allows Come Back's shadow to reduce life while keeping the attack defended" . nightmare $ \pid -> do
    setLife 15
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.comeBack
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    chooseLabel "$label.comeBack.life" pid
    passActions pid
    getRingBearersLife `shouldReturn` 14
    getDamage defender `shouldReturn` 0

  it "allows Come Back's shadow to make the attack undefended without reducing life" . nightmare $ \pid -> do
    setLife 15
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.comeBack
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    chooseLabel "$label.comeBack.undefended" pid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getRingBearersLife `shouldReturn` 15
    getDamage defender `shouldReturn` 2

  it "attaches only one Twilight World to the Ring-bearer while both copies apply doomed" . nightmare $ \pid -> do
    _ <- withRing pid
    setLife 15
    beforeThreat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.theTwilightWorld]
    reveal [Treacheries.theTwilightWorld]
    getsGame (length . (.entities.treacheries)) `shouldReturn` 1
    (.threat) <$> getPlayer pid `shouldReturn` (beforeThreat + 4)

  it "lets The Twilight World exhaust a ready Ring instead of reducing life" . nightmare $ \pid -> do
    _ <- withRing pid
    setLife 15
    reveal [Treacheries.theTwilightWorld]
    run $ CheckWindows [mkAfter $ PhaseBegins PlanningPhase]
    chooseLabel "$label.theTwilightWorld.exhaust" pid
    theOneRingExhausted `shouldReturn` True
    getRingBearersLife `shouldReturn` 15

  it "requires The Twilight World to reduce life when the Ring is exhausted" . nightmare $ \pid -> do
    ring <- withRing pid
    setLife 15
    reveal [Treacheries.theTwilightWorld]
    run $ Exhaust ring
    run $ CheckWindows [mkAfter $ PhaseBegins PlanningPhase]
    getRingBearersLife `shouldReturn` 14

  it "penalizes the Ring-bearer's cancellation of a when-revealed effect" . nightmare $ \pid -> do
    _ <- withRing pid
    setLife 15
    _ <- stageLocation Locations.trollshawForest
    frodo <- toTarget <$> getHeroWithCode (toCardCode Heroes.frodoBagginsTheBlackRiders)
    run $ PlaceTokens GameSource frodo Resource 1
    _ <- setEncounterDeck [Treacheries.theTwilightWorld, Locations.greatEastRoad]
    run RevealEncounterCard
    chooseAbilityOf pid frodo
    resolveRevealed
    getRingBearersLife `shouldReturn` 14

  it "does not penalize the Ring-bearer's cancellation of a card without when-revealed text" . nightmare $ \pid -> do
    _ <- withRing pid
    setLife 15
    _ <- stageLocation Locations.trollshawForest
    frodo <- toTarget <$> getHeroWithCode (toCardCode Heroes.frodoBagginsTheBlackRiders)
    run $ PlaceTokens GameSource frodo Resource 1
    _ <- setEncounterDeck [Locations.greatEastRoad]
    run RevealEncounterCard
    chooseAbilityOf pid frodo
    resolveRevealed
    getRingBearersLife `shouldReturn` 15

  it "offers The Twilight World's choice to the current Ring-bearer controller" . tableNightmare $ \pids -> do
    let p1 = fromJustNote "first" (headMay pids)
        p2 = fromJustNote "second" (headMay $ drop 1 pids)
    _ <- withRing p1
    setLife 15
    reveal [Treacheries.theTwilightWorld]
    run $ SetFirstPlayer p2
    run $ CheckWindows [mkAfter $ PhaseBegins PlanningPhase]
    chooseLabel "$label.theTwilightWorld.life" p2
    getRingBearersLife `shouldReturn` 14

  it "does not let player-card progress explore the Nightmare Ford" . nightmare $ \_ -> do
    lid <- stageLocation Locations.fordOfBruinenNightmare
    hero <- getHeroWithCode "01012"
    run $ PlaceTokens (HeroSource hero.id) (LocationTarget lid) Progress 3
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "does not reduce life for Ettendales' shadow if the defender survives" . nightmare $ \pid -> do
    setLife 15
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Locations.ettendales
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 2
    getRingBearersLife `shouldReturn` 15

  it "has Great East Road's shadow move the attacker to the next player and give it a new shadow" . tableNightmare $ \pids -> do
    let p1 = fromJustNote "first" (headMay pids)
        p2 = fromJustNote "second" (headMay $ drop 1 pids)
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy p1 eid
    defender <-
      getsGame
        ( toTarget
            . fromJustNote "defender"
            . find (\h -> h.owner == p1 && toCardCode h == "01012")
            . toList
            . (.entities.heroes)
        )
    run . AttachShadowCard eid =<< genEncounterCard Locations.greatEastRoad
    _ <- setEncounterDeck [Treacheries.comeBack]
    run $ EnemyAttack p1 eid
    passTableActions
    chooseTarget p1 defender
    passTableActions
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [EngagedWith p2]
    getsGame (map (length . (.shadowCards)) . toList . (.entities.enemies)) `shouldReturn` [2]
