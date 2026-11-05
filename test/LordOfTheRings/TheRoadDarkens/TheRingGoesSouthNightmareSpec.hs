module LordOfTheRings.TheRoadDarkens.TheRingGoesSouthNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer), getAlly)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theRingGoesSouth)
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), modeL, scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport hiding (before)

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) theRingGoesSouth

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

tableNightmare :: Int -> ([PlayerId] -> GameT ()) -> IO ()
tableNightmare n body = do
  seats <-
    traverse
      (\i -> either (error . show) id <$> loadPlayer ("player" <> tshow i) testDeck)
      (1 :| [2 .. n])
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

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

withRing :: GameT ()
withRing = do
  run $ DoStep 3 Setup
  lead <- getLead
  getQuestion lead >>= \case
    Just _ -> void $ answerQuestion lead 0
    Nothing -> pure ()

atStageFour :: GameT ()
atStageFour = do
  run $ DoStep 1 Setup
  withRing
  old <- getActiveQuest
  run $ LeaveQuestStage (toId old)
  card <- genEncounterCard Quests.theGatesOfMoria
  run $ AdvanceToQuest card
  lead <- getLead
  resolveWhenRevealed lead

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

allyFor :: Card -> GameT AllyId
allyFor card =
  getsGame
    ( toId
        . fromJustNote "played ally"
        . find ((== card.id) . allyCardId . toAttrs)
        . toList
        . (.entities.allies)
    )

spec :: Spec
spec = describe "The Ring Goes South Nightmare" do
  it "uses the printed setup removals and sets all four Tentacles aside" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map
      copies
      [ Enemies.crebainFromDunland
      , Enemies.howlingWarg
      , Locations.treeCrownedHill
      , Locations.hillsOfHollin
      , Treacheries.snowdrifts
      , Treacheries.stormOfHowls
      ]
      `shouldBe` [0, 2, 0, 0, 0, 2]
    map
      copies
      [ Enemies.paleGreenTentacle
      , Enemies.huntingHawk
      , Enemies.mistyMountainWarg
      , Locations.expanseOfHollin
      , Locations.slopesOfHithaeglir
      , Treacheries.huntedInHollin
      , Treacheries.shelterlessLand
      ]
      `shouldBe` [0, 2, 3, 3, 3, 2, 2]
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    length (filter ((== toCardCode Enemies.paleGreenTentacle) . toCardCode) aside) `shouldBe` 4

  for_ [1 .. 4] $ \n ->
    it
      ("engages each of " <> show n <> " players with a different Tentacle after the Doors become active")
      . tableNightmare n
      $ \pids -> do
        atStageFour
        tentacles <-
          getsGame
            (filter ((== toCardCode Enemies.paleGreenTentacle) . toCardCode) . toList . (.entities.enemies))
        length tentacles `shouldBe` n
        for_ pids $ \pid -> length (filter ((== EngagedWith pid) . (.placement)) tentacles) `shouldBe` 1
        doors <-
          getsGame
            ( fromJustNote "Doors"
                . find ((== toCardCode Locations.doorsOfDurinTheRingGoesSouth) . toCardCode)
                . toList
                . (.entities.locations)
            )
        getDamage (toTarget doors) `shouldReturn` n
        deck <- getEncounterDeck
        length (filter ((== toCardCode Enemies.paleGreenTentacle) . toCardCode) deck) `shouldBe` (4 - n)
        aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
        any ((== toCardCode Enemies.paleGreenTentacle) . toCardCode) aside `shouldBe` False

  it "keeps the standard scenario's stage four free of Nightmare Tentacles" . gameTestIn theRingGoesSouth testDeck $ \_ -> do
    atStageFour
    getsGame
      ( length
          . filter ((== toCardCode Enemies.paleGreenTentacle) . toCardCode)
          . toList
          . (.entities.enemies)
      )
      `shouldReturn` 0

  it "protects Watcher from damage until every Pale-green Tentacle has left play" . nightmare $ \_ -> do
    watcher <- stageEnemy Enemies.watcherInTheWater
    a <- stageEnemy Enemies.paleGreenTentacle
    b <- stageEnemy Enemies.paleGreenTentacle
    run $ DealDamage GameSource (EnemyTarget watcher) 3
    run $ PlaceTokens GameSource (EnemyTarget watcher) Damage 3
    getDamage (EnemyTarget watcher) `shouldReturn` 0
    run $ Destroy GameSource (EnemyTarget a)
    run $ PlaceTokens GameSource (EnemyTarget watcher) Damage 3
    getDamage (EnemyTarget watcher) `shouldReturn` 0
    run $ Destroy GameSource (EnemyTarget b)
    run $ PlaceTokens GameSource (EnemyTarget watcher) Damage 3
    getDamage (EnemyTarget watcher) `shouldReturn` 3

  it "returns Pale-green Tentacle to staging after killing a character" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.paleGreenTentacle
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [StagingArea]

  it "keeps Pale-green Tentacle engaged when its defender survives" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.paleGreenTentacle
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate defender
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getsGame (map (.placement) . toList . (.entities.enemies)) `shouldReturn` [EngagedWith pid]

  it "forbids Hunting Hawk's attachments and optional engagement" . nightmare $ \_ -> do
    eid <- stageEnemy Enemies.huntingHawk
    getModifiers (EnemyTarget eid)
      >>= ( `shouldSatisfy`
              \ms -> CannotHaveAttachments `elem` ms && CannotBeOptionallyEngagedBy AnyPlayer `elem` ms
          )

  it "can exhaust the ally just played for Hunting Hawk" . nightmare $ \pid -> do
    _ <- stageEnemy Enemies.huntingHawk
    lid <- activeLocation Locations.expanseOfHollin
    card <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    chooseLabel "$label.huntingHawk.exhaust" pid
    ally <- getAlly =<< allyFor card
    allyExhausted (toAttrs ally) `shouldBe` True
    getDamage (LocationTarget lid) `shouldReturn` 0

  it "can damage the active location instead of exhausting the played ally" . nightmare $ \pid -> do
    _ <- stageEnemy Enemies.huntingHawk
    lid <- activeLocation Locations.expanseOfHollin
    card <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    chooseLabel "$label.huntingHawk.damage" pid
    ally <- getAlly =<< allyFor card
    allyExhausted (toAttrs ally) `shouldBe` False
    getDamage (LocationTarget lid) `shouldReturn` 1

  it "does not trigger Hunting Hawk for an ally put into play" . nightmare $ \pid -> do
    _ <- stageEnemy Enemies.huntingHawk
    lid <- activeLocation Locations.expanseOfHollin
    target <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    getDamage (LocationTarget lid) `shouldReturn` 0
    getQuestion pid `shouldReturn` Nothing
    case target of
      AllyTarget aid -> allyExhausted . toAttrs <$> getAlly aid `shouldReturn` False
      _ -> error "not an ally"

  it "requires exhausting the played ally when there is no active location" . nightmare $ \pid -> do
    _ <- stageEnemy Enemies.huntingHawk
    card <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    allyExhausted . toAttrs <$> (getAlly =<< allyFor card) `shouldReturn` True

  it "does not trigger an engaged Hunting Hawk" . nightmare $ \pid -> do
    eid <- stageEnemy Enemies.huntingHawk
    run $ EngageEnemy pid eid
    lid <- activeLocation Locations.expanseOfHollin
    card <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    allyExhausted . toAttrs <$> (getAlly =<< allyFor card) `shouldReturn` False
    getDamage (LocationTarget lid) `shouldReturn` 0

  it "has Misty Mountain Warg's revealed attack damage both active locations after a kill" . nightmare $ \pid -> do
    a <- activeLocation Locations.expanseOfHollin
    b <- activeLocation Locations.slopesOfHithaeglir
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    reveal [Enemies.mistyMountainWarg, Locations.expanseOfHollin]
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage (LocationTarget a) `shouldReturn` 2
    getDamage (LocationTarget b) `shouldReturn` 2

  it
    "does not damage active locations for a surviving defender in Misty Mountain Warg's revealed attack"
    . nightmare
    $ \pid -> do
      a <- activeLocation Locations.expanseOfHollin
      defender <- toTarget <$> getHeroWithCode "01012"
      reveal [Enemies.mistyMountainWarg, Locations.expanseOfHollin]
      passActions pid
      chooseTarget pid defender
      passActions pid
      getDamage (LocationTarget a) `shouldReturn` 0

  it "does not damage active locations for a later ordinary Misty Mountain Warg attack" . nightmare $ \pid -> do
    a <- activeLocation Locations.expanseOfHollin
    eid <- stageEnemy Enemies.mistyMountainWarg
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage (LocationTarget a) `shouldReturn` 0

  it "has Misty Mountain Warg's shadow damage only one chosen active location after a kill" . nightmare $ \pid -> do
    a <- activeLocation Locations.expanseOfHollin
    b <- activeLocation Locations.slopesOfHithaeglir
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . AttachShadowCard eid =<< genEncounterCard Enemies.mistyMountainWarg
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    chooseTarget pid (LocationTarget a)
    getDamage (LocationTarget a) `shouldReturn` 1
    getDamage (LocationTarget b) `shouldReturn` 0

  it "sets Expanse of Hollin's threat from the number of enemies in play" . nightmare $ \pid -> do
    lid <- stageLocation Locations.expanseOfHollin
    getLocationThreat lid `shouldReturn` 1
    _ <- stageEnemy Enemies.huntingHawk
    eid <- stageEnemy Enemies.mistyMountainWarg
    run $ EngageEnemy pid eid
    getLocationThreat lid `shouldReturn` 3

  it "reshuffles then stages every enemy discarded for Expanse of Hollin's damage" . nightmare $ \_ -> do
    lid <- stageLocation Locations.expanseOfHollin
    run $ PlaceTokens GameSource (LocationTarget lid) Damage 3
    _ <- setEncounterDeck [Enemies.huntingHawk, Locations.slopesOfHithaeglir]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.mistyMountainWarg
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
    getsGame (length . (.entities.enemies)) `shouldReturn` 2
    sort
      . map toCardCode
      <$> getEncounterDiscard
      `shouldReturn` sort [toCardCode Locations.slopesOfHithaeglir, toCardCode Locations.expanseOfHollin]

  it "has Slopes of Hithaeglir damage each active location after an actual cancellation" . nightmare $ \pid -> do
    _ <- stageLocation Locations.slopesOfHithaeglir
    a <- activeLocation Locations.expanseOfHollin
    b <- activeLocation Locations.treeCrownedHill
    hero <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.aTestOfWill
    _ <- setEncounterDeck [Treacheries.huntedInHollin]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    getDamage (LocationTarget a) `shouldReturn` 1
    getDamage (LocationTarget b) `shouldReturn` 1

  it "raises each player's threat by the damage on explored Slopes of Hithaeglir" . tableNightmare 2 $ \pids -> do
    lid <- stageLocation Locations.slopesOfHithaeglir
    run $ PlaceTokens GameSource (LocationTarget lid) Damage 3
    before <- traverse (fmap (.threat) . getPlayer) pids
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
    traverse (fmap (.threat) . getPlayer) pids `shouldReturn` map (+ 3) before

  it "has Hunted in Hollin raise threat when there is no enemy to engage" . nightmare $ \pid -> do
    before <- (.threat) <$> getPlayer pid
    reveal [Treacheries.huntedInHollin]
    (.threat) <$> getPlayer pid `shouldReturn` (before + 6)

  it "uses the modified lowest engagement cost for Hunted in Hollin and reveals another card" . nightmare $ \pid -> do
    low <- stageEnemy Enemies.huntingHawk
    high <- stageEnemy Enemies.mistyMountainWarg
    run
      $ CreateEffect
      $ Effect GameSource (EnemyTarget low) [EngagementCostModifier (-20)] UntilEndOfPhase
    reveal [Treacheries.huntedInHollin, Locations.slopesOfHithaeglir]
    chooseLabel "$label.huntedInHollin.engage" pid
    resolveRevealed
    enemies <- getsGame (toList . (.entities.enemies))
    map (.placement) (filter ((== low) . toId) enemies) `shouldBe` [EngagedWith pid]
    map (.placement) (filter ((== high) . toId) enemies) `shouldBe` [StagingArea]
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "does not offer an impossible engagement with Watcher in the Water" . nightmare $ \pid -> do
    _ <- stageEnemy Enemies.watcherInTheWater
    before <- (.threat) <$> getPlayer pid
    reveal [Treacheries.huntedInHollin]
    (.threat) <$> getPlayer pid `shouldReturn` (before + 6)

  it "lets the revealer choose between tied lowest engagement costs" . nightmare $ \pid -> do
    a <- stageEnemy Enemies.mistyMountainWarg
    b <- stageEnemy Enemies.mistyMountainWarg
    reveal [Treacheries.huntedInHollin, Locations.expanseOfHollin]
    chooseLabel "$label.huntedInHollin.engage" pid
    chooseTarget pid (EnemyTarget b)
    resolveRevealed
    enemies <- getsGame (toList . (.entities.enemies))
    map (.placement) (filter ((== b) . toId) enemies) `shouldBe` [EngagedWith pid]
    map (.placement) (filter ((== a) . toId) enemies) `shouldBe` [StagingArea]

  it "recalculates Shelterless Land as active locations and their damage change" . nightmare $ \_ -> do
    a <- activeLocation Locations.expanseOfHollin
    b <- activeLocation Locations.slopesOfHithaeglir
    run $ PlaceTokens GameSource (LocationTarget a) Damage 2
    run $ PlaceTokens GameSource (LocationTarget b) Damage 1
    reveal [Treacheries.shelterlessLand, Treacheries.shelterlessLand, Locations.treeCrownedHill]
    -- Two lasting copies, 3 damage, plus the revealed Hill's 3 threat.
    base <-
      getLocationThreat
        =<< getsGame
          ( toId
              . fromJustNote "Hill"
              . find ((== toCardCode Locations.treeCrownedHill) . toCardCode)
              . toList
              . (.entities.locations)
          )
    getStagingThreat `shouldReturn` (base + 6)
    run $ PlaceTokens GameSource (LocationTarget a) Damage 1
    getStagingThreat `shouldReturn` (base + 8)
    run $ Discard GameSource (LocationTarget b)
    getStagingThreat `shouldReturn` (base + 6)

  it "recalculates Shelterless Land's shadow bonus when active-location damage changes" . nightmare $ \pid -> do
    lid <- activeLocation Locations.expanseOfHollin
    run $ PlaceTokens GameSource (LocationTarget lid) Damage 2
    eid <- stageEnemy Enemies.goblinSniper
    original <- getEnemyAttack eid
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.shelterlessLand
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` (original + 2)
    run $ PlaceTokens GameSource (LocationTarget lid) Damage 1
    getEnemyAttack eid `shouldReturn` (original + 3)
    run $ PlaceLocation lid StagingArea
    getEnemyAttack eid `shouldReturn` original

  it "does not let a second Hunting Hawk exhaust the same ally again" . nightmare $ \pid -> do
    firstHawk <- stageEnemy Enemies.huntingHawk
    secondHawk <- stageEnemy Enemies.huntingHawk
    lid <- activeLocation Locations.expanseOfHollin
    card <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    chooseAbilityOf pid (EnemyTarget firstHawk)
    chooseLabel "$label.huntingHawk.exhaust" pid
    allyExhausted . toAttrs <$> (getAlly =<< allyFor card) `shouldReturn` True
    offersAbilityOf pid (EnemyTarget secondHawk) `shouldReturn` True
    chooseAbilityOf pid (EnemyTarget secondHawk)
    getDamage (LocationTarget lid) `shouldReturn` 1

  it "raises only the revealer's threat for Hunted in Hollin" . tableNightmare 2 $ \pids -> do
    let p1 = fromJustNote "first" (headMay pids)
        p2 = fromJustNote "second" (headMay $ drop 1 pids)
    before1 <- (.threat) <$> getPlayer p1
    before2 <- (.threat) <$> getPlayer p2
    run $ SetRevealer (Just p2)
    reveal [Treacheries.huntedInHollin]
    (.threat) <$> getPlayer p1 `shouldReturn` before1
    (.threat) <$> getPlayer p2 `shouldReturn` (before2 + 6)

  it "ends Shelterless Land's extra staging threat at the end of the phase" . nightmare $ \_ -> do
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    lid <- activeLocation Locations.expanseOfHollin
    run $ PlaceTokens GameSource (LocationTarget lid) Damage 2
    reveal [Treacheries.shelterlessLand, Locations.treeCrownedHill]
    getStagingThreat `shouldReturn` 5
    run $ EndPhase PlanningPhase
    getStagingThreat `shouldReturn` 3

  it "lets Misty Mountain Warg's shadow damage Doors of Durin despite player-card immunity" . nightmare $ \pid -> do
    lid <- activeLocation Locations.doorsOfDurinTheRingGoesSouth
    eid <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . AttachShadowCard eid =<< genEncounterCard Enemies.mistyMountainWarg
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage (LocationTarget lid) `shouldReturn` 1
