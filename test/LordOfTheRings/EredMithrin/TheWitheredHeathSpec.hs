module LordOfTheRings.EredMithrin.TheWitheredHeathSpec (spec) where

import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.EncounterSet (EncounterSet (LostCaves))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (tokensL)
import LordOfTheRings.Entities (enemiesL)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Deep))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theWitheredHeath)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), cavesDeckL, questDeckL, setAsideL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

heathTest :: (PlayerId -> GameT ()) -> IO ()
heathTest = gameTestIn theWitheredHeath testDeck

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

placementOfEnemy :: EnemyId -> GameT (Maybe Placement)
placementOfEnemy eid = getsGame (fmap (.placement) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

activate :: CardDef -> GameT LocationId
activate def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

-- | Damage put straight on the enemy, past what it may take in a round.
wound :: EnemyId -> Int -> GameT ()
wound eid n = putGame . (entitiesL . enemiesL . ix eid %~ overAttrs (tokensL %~ addTokens Damage n)) =<< getGame

setAsideCards :: [CardDef] -> GameT ()
setAsideCards defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (setAsideL .~ cards)) =<< getGame

setAsideCodes :: GameT [CardCode]
setAsideCodes = getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))

setCavesDeck :: [CardDef] -> GameT [EncounterCard]
setCavesDeck defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (cavesDeckL .~ cards)) =<< getGame
  pure cards

cavesCodes :: GameT [CardCode]
cavesCodes = getsGame (map toCardCode . scenarioCavesDeck . toAttrs . (.scenario))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

-- | Dragon Signs in the victory display, as the players earned them.
signsInVictoryDisplay :: Int -> GameT ()
signsInVictoryDisplay n = do
  cards <- replicateM n (EncounterCard <$> genEncounterCard Objectives.dragonSign)
  putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (cards <>))) =<< getGame

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

stagedLocationCount :: GameT Int
stagedLocationCount = getsGame (length . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))

objectivePlacements :: GameT [Placement]
objectivePlacements = getsGame (map (.placement) . toList . (.entities.objectives))

-- | The stage put into play and turned over, its When Revealed resolved.
reachStage :: PlayerId -> CardDef -> GameT Target
reachStage pid def = do
  q <- putQuestStage def
  run $ Flip q
  resolveWhenRevealed pid
  pure q

inCombat :: GameT ()
inCombat = putGame . (phaseL .~ CombatPhase) =<< getGame

-- | The enemy attacks the player, who takes it undefended.
attackUndefended :: PlayerId -> EnemyId -> GameT ()
attackUndefended pid eid = do
  _ <- setEncounterDeck []
  run $ EnemyAttack pid eid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid

spec :: Spec
spec = describe "The Withered Heath" do
  it "is a scenario the game can be dealt" . heathTest $ \_ ->
    isJust (lookupScenario "the-withered-heath") `shouldBe` True

  -- \* Setup and stage 1

  it "sets Cold-Drake and the Dragon Signs aside, makes the Caves deck with 2 of them, and stages a location for each player" . heathTest $ \pid -> do
    flipFirstQuest pid
    aside <- setAsideCodes
    sort aside `shouldBe` sort (map toCardCode [Enemies.coldDrake, Objectives.dragonSign, Objectives.dragonSign])
    caves <- cavesCodes
    length caves `shouldBe` 8
    length (filter (== toCardCode Objectives.dragonSign) caves) `shouldBe` 2
    deck <- getEncounterDeck
    deck `shouldSatisfy` all ((/= Just LostCaves) . cdEncounterSet . toCardDef)
    deck `shouldSatisfy` all ((`notElem` [toCardCode Enemies.coldDrake, toCardCode Objectives.dragonSign]) . toCardCode)
    stagedLocationCount `shouldReturn` 1
    activeQuestCode `shouldReturn` toCardCode Quests.searchingForDragonSign

  it "cannot defeat stage 1 without a Dragon Sign in the victory display" . heathTest $ \pid -> do
    flipFirstQuest pid
    q <- getActiveQuest
    run $ PlaceTokens GameSource (toTarget q) Progress 6
    activeQuestCode `shouldReturn` toCardCode Quests.searchingForDragonSign
    signsInVictoryDisplay 1
    run $ CheckStage (toId q)
    activeQuestCode `shouldReturn` toCardCode Quests.theColdDrakeAttacks

  it "reveals 1 of the top 3 encounter cards after the active location is explored, discarding the rest" . heathTest $ \pid -> do
    flipFirstQuest pid
    lid <- activate Locations.mountainPass
    [_, b, _] <- setEncounterDeck [Treacheries.deadlyCold, Locations.dwarvenDoor, Treacheries.heavySnow]
    run $ PlaceTokens GameSource (toTarget lid) Progress 4
    chooseTarget pid (CardIdTarget b.id)
    door <- locationOf Locations.dwarvenDoor
    getsGame (fmap (.placement) . lookup door . (.entities.locations)) `shouldReturn` Just StagingArea
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` (\d -> all (`elem` d) [toCardCode Treacheries.deadlyCold, toCardCode Treacheries.heavySnow]))

  -- \* Dragon Sign and the Caves deck

  it "brings a Dragon Sign by the Deep keyword to the staging area guarded by an enemy, and adds it to the victory display once free" . heathTest $ \_ -> do
    void $ setCavesDeck [Objectives.dragonSign]
    _ <- setEncounterDeck [Treacheries.deadlyCold, Enemies.dragonHatchling]
    door <- stageLocation Locations.dwarvenDoor
    run $ TravelTo door
    objectivePlacements `shouldReturn` [StagingArea]
    hatchling <- enemyOf Enemies.dragonHatchling
    placementOfEnemy hatchling `shouldReturn` Just StagingArea
    getModifiers hatchling >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    sort . map toCardCode <$> getEncounterDiscard `shouldReturn` sort [toCardCode Treacheries.deadlyCold, toCardCode Locations.dwarvenDoor]
    run $ Destroy GameSource (EnemyTarget hatchling)
    victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Objectives.dragonSign))
    objectivePlacements `shouldReturn` []

  it "attaches a Dragon Sign from the Caves deck to Creature Den, which is immune, and adds it to the victory display as the Den is explored" . heathTest $ \pid -> do
    void $ setCavesDeck [Locations.lostArmory, Objectives.dragonSign]
    _ <- setEncounterDeck [Treacheries.deadlyCold, Enemies.snowTroll]
    den <- stageLocation Locations.creatureDen
    run $ TravelTo den
    chooseAbilityOf pid den
    chooseLabel "$label.pass" pid
    objectivePlacements `shouldReturn` [AttachedTo (LocationTarget den)]
    cavesCodes `shouldReturn` [toCardCode Locations.lostArmory]
    getModifiers den >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    troll <- enemyOf Enemies.snowTroll
    placementOfEnemy troll `shouldReturn` Just StagingArea
    run $ PlaceTokens GameSource (toTarget den) Progress 4
    victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Objectives.dragonSign))

  -- \* Stage 2

  it "adds Cold-Drake to the staging area at stage 2, engaged with each player, ignoring Deep" . heathTest $ \pid -> do
    setAsideCards [Enemies.coldDrake]
    _ <- reachStage pid Quests.theColdDrakeAttacks
    drake <- enemyOf Enemies.coldDrake
    placementOfEnemy drake `shouldReturn` Just StagingArea
    getModifiers drake >>= (`shouldSatisfy` elem (EngagedWithEachPlayer AnyPlayer))
    getModifiers GameTarget >>= (`shouldSatisfy` elem IgnoreDeep)
    run $ EngageEnemy pid drake
    placementOfEnemy drake `shouldReturn` Just StagingArea

  it "places no more than 6 damage on Cold-Drake each round and no more than 18 in all, which do not destroy it" . heathTest $ \pid -> do
    setAsideCards [Enemies.coldDrake]
    _ <- reachStage pid Quests.theColdDrakeAttacks
    drake <- enemyOf Enemies.coldDrake
    run $ PlaceTokens GameSource (toTarget drake) Damage 8
    getDamage (toTarget drake) `shouldReturn` 6
    wound drake 9
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource (toTarget drake) Damage 5
    getDamage (toTarget drake) `shouldReturn` 18
    run $ CheckDefeated GameSource (toTarget drake)
    placementOfEnemy drake `shouldReturn` Just StagingArea

  it "cannot defeat stage 2 while Cold-Drake has hit points left" . heathTest $ \pid -> do
    setAsideCards [Enemies.coldDrake]
    next <- genEncounterCard Quests.huntingTheBeast
    putGame . (scenarioL %~ overAttrs (questDeckL .~ [next])) =<< getGame
    q <- reachStage pid Quests.theColdDrakeAttacks
    drake <- enemyOf Enemies.coldDrake
    run $ PlaceTokens GameSource q Progress 3
    activeQuestCode `shouldReturn` toCardCode Quests.theColdDrakeAttacks
    wound drake 17
    run $ PlaceTokens GameSource (toTarget drake) Damage 1
    activeQuestCode `shouldReturn` toCardCode Quests.huntingTheBeast

  -- \* Stage 3

  it "sets Cold-Drake aside with its damage at stage 3, shuffles 2 Dragon Signs into the Caves deck, and stages a location for each player" . heathTest $ \pid -> do
    drake <- stage Enemies.coldDrake
    wound drake 18
    setAsideCards [Objectives.dragonSign, Objectives.dragonSign]
    void $ setCavesDeck []
    _ <- setEncounterDeck [Treacheries.deadlyCold, Locations.highFalls]
    _ <- reachStage pid Quests.huntingTheBeast
    placementOfEnemy drake `shouldReturn` Nothing
    setAsideCodes `shouldReturn` [toCardCode Enemies.coldDrake]
    cavesCodes `shouldReturn` replicate 2 (toCardCode Objectives.dragonSign)
    falls <- locationOf Locations.highFalls
    getsGame (fmap (.placement) . lookup falls . (.entities.locations)) `shouldReturn` Just StagingArea

  it "removes 3 damage from the set-aside Cold-Drake at the end of each round, and stage 4 brings it back with the rest" . heathTest $ \pid -> do
    drake <- stage Enemies.coldDrake
    wound drake 18
    _ <- setEncounterDeck [Locations.highFalls]
    _ <- reachStage pid Quests.huntingTheBeast
    run $ CheckWindows [mkAfter RoundEnds]
    run $ CheckWindows [mkAfter RoundEnds]
    signsInVictoryDisplay 3
    hunt <- getActiveQuest
    next <- genEncounterCard Quests.aCorneredDragon
    putGame . (scenarioL %~ overAttrs (questDeckL .~ [next])) =<< getGame
    run $ PlaceTokens GameSource (toTarget hunt) Progress 9
    resolveWhenRevealed pid
    back <- enemyOf Enemies.coldDrake
    getDamage (toTarget back) `shouldReturn` 12

  it "cannot defeat stage 3 with fewer than 3 Dragon Signs in the victory display" . heathTest $ \pid -> do
    _ <- setEncounterDeck [Locations.highFalls]
    q <- reachStage pid Quests.huntingTheBeast
    signsInVictoryDisplay 2
    run $ PlaceTokens GameSource q Progress 9
    activeQuestCode `shouldReturn` toCardCode Quests.huntingTheBeast
    getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)

  -- \* Stage 4

  it "holds Cold-Drake to X damage a round at stage 4, X the Dragon Signs in the victory display, and the players win as it is defeated" . heathTest $ \pid -> do
    setAsideCards [Enemies.coldDrake]
    signsInVictoryDisplay 3
    _ <- reachStage pid Quests.aCorneredDragon
    -- its immediate attack, taken undefended
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid =<< hero "01002"
    passActions pid
    drake <- enemyOf Enemies.coldDrake
    run $ PlaceTokens GameSource (toTarget drake) Damage 5
    getDamage (toTarget drake) `shouldReturn` 3
    getsGame (.outcome) `shouldReturn` Nothing
    wound drake 14
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource (toTarget drake) Damage 1
    run $ CheckDefeated GameSource (toTarget drake)
    getsGame (.outcome) `shouldReturn` Just Won

  -- \* Encounter cards

  it "gives Cold-Drake, Dragon Hatchling and Cave Entrance more for each Dragon Sign in the victory display" . heathTest $ \_ -> do
    signsInVictoryDisplay 2
    drake <- stage Enemies.coldDrake
    hatchling <- stage Enemies.dragonHatchling
    entrance <- stageLocation Locations.caveEntrance
    getEnemyThreat drake `shouldReturn` 3
    getEnemyDefense drake `shouldReturn` 3
    getEnemyAttack hatchling `shouldReturn` 4
    getEnemyDefense hatchling `shouldReturn` 2
    getLocationThreat entrance `shouldReturn` 3
    getModifiers drake >>= (`shouldSatisfy` (\ms -> all (`elem` ms) [CannotHaveAttachments, CannotTakeNonCombatDamage]))

  it "keeps the defender of Cold-Drake from readying this round when its shadow card has no effect" . heathTest $ \pid -> do
    drake <- engaged pid Enemies.coldDrake
    _ <- setEncounterDeck [Locations.mountainPass]
    run $ DealShadowCard drake
    inCombat
    run $ EnemyAttacks pid
    passActions pid
    beravor <- hero "01012"
    chooseTarget pid beravor
    passActions pid
    getModifiers beravor >>= (`shouldSatisfy` elem CannotReady)

  it "returns Snow-troll to the staging area after its attack destroys a character" . heathTest $ \pid -> do
    troll <- engaged pid Enemies.snowTroll
    beravor <- hero "01012"
    run $ PlaceTokens GameSource beravor Damage 3
    attackUndefended pid troll
    chooseTarget pid beravor
    passActions pid
    placementOfEnemy troll `shouldReturn` Just StagingArea

  it "gives the attacking enemy Dragon Hatchling's shadow bonus for the Dragon Signs in the victory display" . heathTest $ \pid -> do
    signsInVictoryDisplay 1
    troll <- engaged pid Enemies.snowTroll
    _ <- setEncounterDeck [Enemies.dragonHatchling]
    run $ DealShadowCard troll
    inCombat
    run $ EnemyAttacks pid
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    getEnemyAttack troll `shouldReturn` 6
    getEnemyDefense troll `shouldReturn` 4

  it "keeps Weather treacheries' when revealed effects from being canceled while Mountain Pass is in the staging area" . heathTest $ \pid -> do
    _ <- stageLocation Locations.mountainPass
    reveal [Treacheries.heavySnow]
    snow <- getsGame (fromJustNote "no treachery" . fmap toId . headMay . toList . (.entities.treacheries))
    getModifiers (TreacheryTarget snow) >>= (`shouldSatisfy` elem WhenRevealedCannotBeCanceled)
    _ <- pure pid
    pure ()

  it "takes no progress on Dwarven Door in the staging area" . heathTest $ \_ -> do
    door <- stageLocation Locations.dwarvenDoor
    getModifiers door >>= (`shouldSatisfy` elem (ProgressLimit 0))

  it "raises each player's threat by 1 to travel to Cave Entrance" . heathTest $ \pid -> do
    threat0 <- (.threat) <$> getPlayer pid
    entrance <- stageLocation Locations.caveEntrance
    lasting (LocationTarget entrance) (LoseKeyword Deep)
    run $ TravelTo entrance
    (.threat) <$> getPlayer pid `shouldReturn` (threat0 + 1)

  it "gives High Falls the locations in the staging area as threat, and assigns as much damage to travel there" . heathTest $ \pid -> do
    falls <- stageLocation Locations.highFalls
    _ <- stageLocation Locations.mountainPass
    getLocationThreat falls `shouldReturn` 2
    lasting (LocationTarget falls) (LoseKeyword Deep)
    run $ TravelTo falls
    beravor <- hero "01012"
    chooseTarget pid beravor
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2

  it "has Heavy Snow exhaust a character and bar player cards from readying characters this round" . heathTest $ \pid -> do
    reveal [Treacheries.heavySnow]
    beravor <- hero "01012"
    chooseTarget pid beravor
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotBeReadiedByPlayerCards)

  it "has Deadly Cold make each player exhaust or damage each ready character he controls" . heathTest $ \pid -> do
    reveal [Treacheries.deadlyCold]
    chooseLabel "$label.deadlyCold.exhaust" pid
    chooseLabel "$label.deadlyCold.damage" pid
    chooseLabel "$label.deadlyCold.damage" pid
    exhausted <- length . filter (heroExhausted . toAttrs) . toList <$> getsGame (.entities.heroes)
    exhausted `shouldBe` 1
    damaged <- filterM (fmap (> 0) . getDamage . toTarget) . toList =<< getsGame (.entities.heroes)
    length damaged `shouldBe` 2
 where
  lasting t m = run $ CreateEffect $ Effect GameSource t [m] UntilEndOfRound
