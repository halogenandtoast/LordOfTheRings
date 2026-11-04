module LordOfTheRings.EredMithrin.FireInTheNightSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getDefenseAgainst)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode), PlayerMatcher (AnyPlayer), getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (EncounterPhase, PlanningPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (fireInTheNight, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (sideQuestDeckOf)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), metaL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

fireTest :: (PlayerId -> GameT ()) -> IO ()
fireTest = gameTestIn fireInTheNight testDeck

questOf :: CardDef -> GameT Target
questOf def = getsGame (toTarget . fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questCodes :: GameT [CardCode]
questCodes = getsGame (map toCardCode . toList . (.entities.quests))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | Hrogar's Hill turned to its B side, with Dagnir in the staging area.
atTheHill :: GameT (Target, EnemyId)
atTheHill = do
  _ <- setEncounterDeck []
  hill <- putQuestStage Quests.hrogarsHill
  run $ Flip hill
  dagnir <- stage Enemies.dagnir
  pure (hill, dagnir)

-- | Side quests in the victory display, as defeated ones are.
defeatedSideQuests :: Int -> GameT ()
defeatedSideQuests n = do
  cards <- traverse genEncounterCard (take n [Quests.drawHerFire, Quests.holdTheDoor, Quests.faceTheDragon])
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (victoryDisplayL .~ map EncounterCard cards)
  putGame =<< preloadModifiers =<< getGame

sideQuest :: CardDef -> GameT Target
sideQuest def = do
  run . RevealCard =<< genEncounterCard def
  resolveRevealed
  questOf def

phaseEnds :: Phase -> GameT ()
phaseEnds = run . CheckWindows . pure . mkWhen . PhaseEnds

-- | The quest card as the source of an effect, as Face the Dragon's damage.
questSource :: Target -> Source
questSource = \case
  QuestTarget q -> QuestSource q
  t -> error $ "not a quest: " <> show t

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

spec :: Spec
spec = describe "Fire in the Night" do
  it "is a scenario the game can be dealt" . fireTest $ \_ ->
    isJust (lookupScenario "fire-in-the-night") `shouldBe` True

  -- \* Setup

  it "sets the 8 side quests aside as the side quest deck and adds Dagnir to the staging area" . fireTest $ \pid -> do
    flipFirstQuest pid
    sideQuests <- getsGame (sideQuestDeckOf . scenarioMeta . toAttrs . (.scenario))
    sort (map toCardCode sideQuests)
      `shouldBe` sort (map toCardCode [Quests.drawHerFire, Quests.fortifyTheDefense, Quests.rallyTheWoodmen, Quests.douseTheFlames, Quests.holdTheDoor, Quests.defendTheTown, Quests.routTheGoblins, Quests.faceTheDragon])
    dagnir <- enemyOf Enemies.dagnir
    ((.placement) <$> getEnemy dagnir) `shouldReturn` StagingArea
    deck <- getEncounterDeck
    filter ((`elem` map toCardCode [Enemies.dagnir, Quests.drawHerFire]) . toCardCode) deck `shouldBe` []
    questCodes `shouldReturn` [toCardCode Quests.hrogarsHill]

  it "has each other player reveal an encounter card at setup" . twoPlayerTestIn fireInTheNight testDeck $ \p1 p2 -> do
    run Setup
    -- each keeps his hand, whoever is asked first
    replicateM_ 2 do
      asked <- filterM (fmap isJust . getQuestion) [p1, p2]
      for_ (take 1 asked) $ chooseLabel "$label.keepHand"
    lead <- getLead
    deck0 <- length <$> getEncounterDeck
    chooseOptionMatching "flip the quest" lead (const True)
    resolveRevealed
    deck1 <- length <$> getEncounterDeck
    -- Dagnir leaves the deck, and the second player's card is revealed
    deck1 `shouldBe` deck0 - 2

  -- \* Hrogar's Hill

  it "reveals a side quest and has Dagnir attack Hrogar's Hill at the end of the planning phase" . fireTest $ \pid -> do
    (hill, _) <- atTheHill
    sideQuestCard <- genEncounterCard Quests.douseTheFlames
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (metaL .~ object ["sideQuestDeck" .= [sideQuestCard]])
    phaseEnds PlanningPhase
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    getDamage hill `shouldReturn` 7
    questCodes >>= (`shouldSatisfy` elem (toCardCode Quests.douseTheFlames))
    getsGame (sideQuestDeckOf . scenarioMeta . toAttrs . (.scenario)) `shouldReturn` []

  it "deals the excess damage of Dagnir's attack on the hill to Hrogar's Hill" . fireTest $ \pid -> do
    (hill, _) <- atTheHill
    phaseEnds PlanningPhase
    passActions pid
    beravor <- hero "01012"
    chooseTarget pid beravor
    passActions pid
    -- 7 attack against Beravor's 2 defense and 4 hit points
    getDamage hill `shouldReturn` 1

  it "loses the game as Hrogar's Hill has damage equal to its quest points" . fireTest $ \_ -> do
    (hill, _) <- atTheHill
    run $ DealDamage GameSource hill 49
    outcome `shouldReturn` Nothing
    run $ DealDamage GameSource hill 1
    outcome `shouldReturn` Just Lost

  it "cannot be defeated by progress, and wins the game once Dagnir has no hit points remaining" . fireTest $ \_ -> do
    (hill, dagnir) <- atTheHill
    run $ PlaceTokens GameSource hill Progress 50
    outcome `shouldReturn` Nothing
    questCodes `shouldReturn` [toCardCode Quests.hrogarsHill]
    run $ DealDamage (questSource hill) (EnemyTarget dagnir) 20
    outcome `shouldReturn` Just Won

  -- \* Dagnir

  it "takes no more than X damage from non-quest effects each round" . fireTest $ \pid -> do
    (_, dagnir) <- atTheHill
    run $ DealDamage (PlayerAttackSource pid) (EnemyTarget dagnir) 5
    ((.damage) <$> getEnemy dagnir) `shouldReturn` 1
    defeatedSideQuests 1
    run $ DealDamage (PlayerAttackSource pid) (EnemyTarget dagnir) 5
    ((.damage) <$> getEnemy dagnir) `shouldReturn` 2
    run $ DealDamage (PlayerAttackSource pid) (EnemyTarget dagnir) 5
    ((.damage) <$> getEnemy dagnir) `shouldReturn` 2

  it "is immune to player card effects, and engaged with each player at 3 side quests in the victory display" . fireTest $ \_ -> do
    (_, dagnir) <- atTheHill
    mods <- getModifiers (EnemyTarget dagnir)
    mods `shouldSatisfy` elem ImmuneToPlayerCardEffects
    mods `shouldSatisfy` elem CannotLeaveStagingArea
    mods `shouldSatisfy` notElem (EngagedWithEachPlayer AnyPlayer)
    defeatedSideQuests 3
    getModifiers (EnemyTarget dagnir) >>= (`shouldSatisfy` elem (EngagedWithEachPlayer AnyPlayer))

  -- \* Side quests

  it "has Dagnir attack the first player as Draw Her Fire becomes the current quest" . fireTest $ \pid -> do
    _ <- atTheHill
    q <- sideQuest Quests.drawHerFire
    run $ SetCurrentQuest q
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` isJust)
    chooseLabel "$label.takeAttackUndefended" pid

  it "has characters not count their defense against Dagnir for Fortify the Defense, and lets each player draw 3 as it is defeated" . fireTest $ \pid -> do
    (_, dagnir) <- atTheHill
    beravor <- hero "01012"
    getDefenseAgainst dagnir beravor `shouldReturn` 2
    q <- sideQuest Quests.fortifyTheDefense
    getDefenseAgainst dagnir beravor `shouldReturn` 0
    hand0 <- length . (.hand) <$> getPlayer pid
    run $ PlaceTokens GameSource q Progress 10
    chooseLabel "$label.fortifyTheDefense.draw" pid
    (length . (.hand) <$> getPlayer pid) `shouldReturn` (hand0 + 3)

  it "bars allies from attacking Dagnir for Rally the Woodmen" . fireTest $ \pid -> do
    _ <- atTheHill
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- sideQuest Quests.rallyTheWoodmen
    getModifiers ally >>= (`shouldSatisfy` elem (CannotAttackMatching (EnemyWithCardCode $ toCardCode Enemies.dagnir)))

  it "deals 5 damage to Hrogar's Hill at the end of the quest phase for Douse the Flames, and removes 10 as it is defeated" . fireTest $ \_ -> do
    (hill, _) <- atTheHill
    q <- sideQuest Quests.douseTheFlames
    run $ DealDamage GameSource hill 7
    phaseEnds QuestPhase
    getDamage hill `shouldReturn` 12
    run $ PlaceTokens GameSource q Progress 10
    getDamage hill `shouldReturn` 2

  it "keeps threat from being reduced for Hold the Door, and reduces it by 5 as it is defeated" . fireTest $ \pid -> do
    _ <- atTheHill
    q <- sideQuest Quests.holdTheDoor
    run $ RaiseThreat pid 20
    threat0 <- (.threat) <$> getPlayer pid
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotReduceThreat)
    run $ PlaceTokens GameSource q Progress 10
    ((.threat) <$> getPlayer pid) `shouldReturn` (threat0 - 5)

  it "reveals an additional card while Defend the Town is the current quest, and damages the hill for each staged enemy" . fireTest $ \_ -> do
    (hill, _) <- atTheHill
    q <- sideQuest Quests.defendTheTown
    run $ SetCurrentQuest q
    putGame =<< preloadModifiers =<< getGame
    getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))
    _ <- stage Enemies.orcOfTheFlame
    phaseEnds EncounterPhase
    getDamage hill `shouldReturn` 2

  it "puts a different Goblin enemy into play engaged with each player for Rout the Goblins" . twoPlayerTestIn fireInTheNight testDeck $ \p1 p2 -> do
    _ <- atTheHill
    _ <- setEncounterDeck [Enemies.orcOfTheFlame, Enemies.orcOfTheFlame, Enemies.goblinTroop]
    q <- genEncounterCard Quests.routTheGoblins
    run $ RevealCard q
    resolveRevealed
    lead <- getLead
    let other = if lead == p1 then p2 else p1
    orc <- getsGame (fromJustNote "no Orc of the Flame" . find ((== toCardCode Enemies.orcOfTheFlame) . toCardCode) . (.encounterDeck) . toAttrs . (.scenario))
    chooseTarget lead orc
    engaged <- getsGame (map (\e -> (toCardCode e, (toAttrs e).placement)) . filter ((/= toCardCode Enemies.dagnir) . toCardCode) . toList . (.entities.enemies))
    sort engaged `shouldBe` sort [(toCardCode Enemies.orcOfTheFlame, EngagedWith lead), (toCardCode Enemies.goblinTroop, EngagedWith other)]

  it "deals Dagnir 5 damage past her limit as Face the Dragon is defeated, and heals her as her attack destroys a character" . fireTest $ \pid -> do
    (_, dagnir) <- atTheHill
    q <- sideQuest Quests.faceTheDragon
    run $ DealDamage (PlayerAttackSource pid) (EnemyTarget dagnir) 1
    run $ PlaceTokens GameSource q Progress 10
    ((.damage) <$> getEnemy dagnir) `shouldReturn` 6

  it "heals Dagnir 5 for Face the Dragon as her attack destroys a character" . fireTest $ \pid -> do
    (hill, dagnir) <- atTheHill
    _ <- sideQuest Quests.faceTheDragon
    run $ DealDamage (questSource hill) (EnemyTarget dagnir) 6
    phaseEnds PlanningPhase
    passActions pid
    beravor <- hero "01012"
    chooseTarget pid beravor
    passActions pid
    ((.damage) <$> getEnemy dagnir) `shouldReturn` 1

  -- \* Encounter cards

  it "deals 3 damage to Hrogar's Hill as The Dragon's Thrall engages, when the player chooses so" . fireTest $ \pid -> do
    (hill, _) <- atTheHill
    thrall <- stage Enemies.theDragonsThrall
    run $ EngageEnemy pid thrall
    chooseLabel "$label.theDragonsThrall.damage" pid
    getDamage hill `shouldReturn` 3

  it "has the first player damage Hrogar's Hill for Orc of the Flame, and gives +2 attack for its shadow at 25 damage" . fireTest $ \pid -> do
    (hill, _) <- atTheHill
    reveal [Enemies.orcOfTheFlame]
    chooseLabel "$label.orcOfTheFlame.hill" pid
    getDamage hill `shouldReturn` 3
    run $ DealDamage GameSource hill 22
    thrall <- stage Enemies.theDragonsThrall
    run $ EngageEnemy pid thrall
    chooseLabel "$label.theDragonsThrall.damage" pid
    _ <- setEncounterDeck [Enemies.orcOfTheFlame]
    run $ DealShadowCard thrall
    cards <- (.shadowCards) <$> getEnemy thrall
    for_ cards \c -> run $ ResolveShadowCard c.id thrall pid []
    getEnemyAttack thrall `shouldReturn` (3 + 2)

  it "discards Town Gate with 4 damage to the hill at its third quest phase in the staging area" . fireTest $ \_ -> do
    (hill, _) <- atTheHill
    _ <- stageLocation Locations.townGate
    replicateM_ 3 $ phaseEnds QuestPhase
    getDamage hill `shouldReturn` 4
    getsGame (length . (.entities.locations)) `shouldReturn` 0

  it "gives Dagnir -2 attack while Wooden Palisade is active" . fireTest $ \_ -> do
    (_, dagnir) <- atTheHill
    palisade <- stageLocation Locations.woodenPalisade
    beravor <- hero "01012"
    run $ Exhaust beravor
    run $ TravelTo palisade
    -- Beravor, the one hero exhausted, is readied without asking
    (heroExhausted . toAttrs <$> getHeroWithCode "01012") `shouldReturn` False
    getEnemyAttack dagnir `shouldReturn` 5

  it "has each player assign X damage as they travel to Burning Watchtower" . fireTest $ \pid -> do
    _ <- atTheHill
    tower <- stageLocation Locations.burningWatchtower
    run $ TravelTo tower
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1

  it "discards the active location for Powerful in Wrath, damaging the hill by its threat, and surges without one" . fireTest $ \_ -> do
    (hill, _) <- atTheHill
    palisade <- stageLocation Locations.woodenPalisade
    run $ TravelTo palisade
    reveal [Treacheries.powerfulInWrath]
    getDamage hill `shouldReturn` 2
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    reveal [Treacheries.powerfulInWrath, Locations.burningWatchtower]
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.burningWatchtower]

  it "has Dagnir attack the first player as they travel to Town Gate" . fireTest $ \pid -> do
    _ <- atTheHill
    gate <- stageLocation Locations.townGate
    run $ TravelTo gate
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    beravor <- hero "01012"
    chooseTarget pid beravor
    passActions pid
    getsGame (map (\l -> (toAttrs l).placement) . toList . (.entities.locations)) `shouldReturn` [ActiveLocation]

  it "deals the undefended damage of Dagnir's attack to Hrogar's Hill for The Dragon's Fury" . fireTest $ \pid -> do
    (hill, _) <- atTheHill
    _ <- setEncounterDeck [Treacheries.theDragonsFury, Locations.woodenPalisade]
    run RevealEncounterCard
    resolveRevealed
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    getDamage hill `shouldReturn` 7

  it "has each player assign X damage for Bright Flames, and gives +1 attack for each side quest in the victory display for its shadow" . fireTest $ \pid -> do
    _ <- atTheHill
    defeatedSideQuests 2
    reveal [Treacheries.brightFlames]
    beravor <- hero "01012"
    replicateM_ 3 $ chooseTarget pid beravor
    getDamage beravor `shouldReturn` 3
    thrall <- stage Enemies.theDragonsThrall
    run $ EngageEnemy pid thrall
    chooseLabel "$label.theDragonsThrall.damage" pid
    _ <- setEncounterDeck [Treacheries.brightFlames]
    run $ DealShadowCard thrall
    cards <- (.shadowCards) <$> getEnemy thrall
    for_ cards \c -> run $ ResolveShadowCard c.id thrall pid []
    getEnemyAttack thrall `shouldReturn` (3 + 2)
