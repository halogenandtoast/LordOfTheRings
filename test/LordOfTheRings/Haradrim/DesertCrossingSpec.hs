module LordOfTheRings.Haradrim.DesertCrossingSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEngagementCost, getLocationThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (desertCrossing, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (getTemperature)
import LordOfTheRings.Scenario.Types (encounterDiscardL, metaL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

desertTest :: (PlayerId -> GameT ()) -> IO ()
desertTest = gameTestIn desertCrossing testDeck

setTemperature :: Int -> GameT ()
setTemperature n = do
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (metaL .~ object ["temperature" .= n])

roundEnds :: GameT ()
roundEnds = run $ CheckWindows [mkWhen RoundEnds]

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

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

locationsIn :: GameT [(CardCode, Placement)]
locationsIn = getsGame (map (\l -> (toCardCode l, (toAttrs l).placement)) . toList . (.entities.locations))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | The stage put into play turned over, as it is once advanced to.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  q <- putQuestStage def
  run $ Flip q
  pure q

-- | The stage advanced to, its When Revealed effect resolved.
revealedStage :: PlayerId -> CardDef -> GameT ()
revealedStage pid def = do
  card <- genEncounterCard def
  run $ AdvanceToQuest card
  resolveWhenRevealed pid

isQuestion :: Text -> Maybe (Question msg) -> Bool
isQuestion key = \case
  Just (QuestionLabel l _ _ _) -> key `isPrefixOf` l
  _ -> False

-- | The shadow card dealt to the enemy from the top of the encounter deck, resolved
-- against the player.
resolveShadow :: PlayerId -> EnemyId -> CardDef -> GameT ()
resolveShadow pid eid def = do
  _ <- setEncounterDeck [def]
  run $ DealShadowCard eid
  cards <- (.shadowCards) <$> getEnemy eid
  for_ cards \c -> run $ ResolveShadowCard c.id eid pid []

spec :: Spec
spec = describe "Desert Crossing" do
  it "is a scenario the game can be dealt" . desertTest $ \_ ->
    isJust (lookupScenario "desert-crossing") `shouldBe` True

  it "adds a Burnt Sands for each player to the staging area and sets the temperature to 10" . desertTest $ \pid -> do
    flipFirstQuest pid
    locationsIn `shouldReturn` [(toCardCode Locations.burntSands, StagingArea)]
    getTemperature `shouldReturn` 10
    deck <- getEncounterDeck
    length (filter ((== toCardCode Locations.burntSands) . toCardCode) deck) `shouldBe` 3
    -- Desert Crossing, Desert Creatures and Desert Sands, less the Burnt Sands staged
    length deck `shouldBe` 36
    (toCardCode <$> getActiveQuest) `shouldReturn` toCardCode Quests.theDesertOfHarad

  -- \* The heat tracker

  it "loses the game as the temperature reaches 60" . desertTest $ \_ -> do
    setTemperature 56
    run $ ScenarioSpecific "desertCrossing.temperature" (toJSON (2 :: Int))
    getsGame (.outcome) `shouldReturn` Nothing
    run $ ScenarioSpecific "desertCrossing.temperature" (toJSON (2 :: Int))
    getTemperature `shouldReturn` 60
    getsGame (.outcome) `shouldReturn` Just Lost

  -- \* The Desert of Harad

  it "has the player exhaust an ally or deal it 1 damage as it enters play" . desertTest $ \pid -> do
    _ <- flippedStage Quests.theDesertOfHarad
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    chooseLabel "$label.theDesertOfHarad.damage" pid
    getDamage ally `shouldReturn` 1

  it "raises the temperature by 4 at the end of the round" . desertTest $ \_ -> do
    _ <- flippedStage Quests.theDesertOfHarad
    setTemperature 10
    roundEnds
    getTemperature `shouldReturn` 14

  -- \* Under the Merciless Sun

  it "raises the temperature by 2, shuffles the discard pile into the deck and reveals a card per player" . desertTest $ \pid -> do
    setTemperature 20
    discarded <- genEncounterCard Locations.parchedWadi
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [discarded])
    _ <- setEncounterDeck []
    revealedStage pid Quests.underTheMercilessSun
    getTemperature `shouldReturn` 22
    locationsIn `shouldReturn` [(toCardCode Locations.parchedWadi, StagingArea)]

  it "raises the temperature by 4 at the end of the round, then has each player assign X damage" . desertTest $ \pid -> do
    _ <- setEncounterDeck []
    revealedStage pid Quests.underTheMercilessSun
    setTemperature 26
    roundEnds
    getTemperature `shouldReturn` 30
    beravor <- hero "01012"
    replicateM_ 3 $ chooseTarget pid beravor
    getDamage beravor `shouldReturn` 3

  -- \* Beasts of Legend

  it "raises the temperature by 2 and adds a Were-worm from the discard pile to the staging area" . desertTest $ \pid -> do
    setTemperature 30
    worm <- genEncounterCard Enemies.wereWorm
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [worm])
    _ <- setEncounterDeck [Locations.burntSands]
    revealedStage pid Quests.beastsOfLegend
    getTemperature `shouldReturn` 32
    enemyCodes `shouldReturn` [toCardCode Enemies.wereWorm]

  it "cannot be defeated while a Were-worm is in play, and wins the game once it is gone" . desertTest $ \pid -> do
    _ <- setEncounterDeck []
    revealedStage pid Quests.beastsOfLegend
    q <- toTarget <$> getActiveQuest
    worm <- stage Enemies.wereWorm
    run $ PlaceTokens GameSource q Progress 16
    getsGame (.outcome) `shouldReturn` Nothing
    run $ Discard (toTarget worm)
    getsGame (.outcome) `shouldReturn` Just Won

  it "adds X threat to the staging area during the quest phase" . desertTest $ \pid -> do
    _ <- setEncounterDeck []
    revealedStage pid Quests.beastsOfLegend
    setTemperature 40
    threat0 <- getStagingThreat
    putGame . (phaseL .~ QuestPhase) =<< getGame
    putGame =<< preloadModifiers =<< getGame
    getStagingThreat `shouldReturn` (threat0 + 4)

  -- \* Seek Shade

  it "removes X progress from the main quest at the end of the round, and cools by 10 as it is defeated" . desertTest $ \_ -> do
    main' <- putQuestStage Quests.theDesertOfHarad
    run $ PlaceTokens GameSource main' Progress 5
    setTemperature 30
    reveal [Quests.seekShade, Locations.burntSands]
    roundEnds
    getsGame (fmap (.progress) . find ((== toCardCode Quests.theDesertOfHarad) . toCardCode) . toList . (.entities.quests))
      `shouldReturn` Just 2
    shade <- getsGame (toTarget . fromJustNote "no Seek Shade" . find ((== toCardCode Quests.seekShade) . toCardCode) . toList . (.entities.quests))
    run $ PlaceTokens GameSource shade Progress 6
    getTemperature `shouldReturn` 20

  -- \* Enemies

  it "gives Were-worm X attack and defense, and an immediate attack with no shadow card as it engages" . desertTest $ \pid -> do
    setTemperature 30
    worm <- stage Enemies.wereWorm
    getEnemyAttack worm `shouldReturn` 3
    getEnemyDefense worm `shouldReturn` 3
    _ <- setEncounterDeck [Locations.burntSands]
    run $ EngageEnemy pid worm
    ((.shadowCards) <$> getEnemy worm) `shouldReturn` []
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid

  it "gives the attacking enemy +2 attack for Were-worm's shadow at 40 or higher" . desertTest $ \pid -> do
    setTemperature 40
    troll <- stage Enemies.hillTroll
    run $ EngageEnemy pid troll
    resolveShadow pid troll Enemies.wereWorm
    getEnemyAttack troll `shouldReturn` (6 + 2)

  it "lowers Carrion Bird's engagement cost for each damaged character, and discards one as it engages" . desertTest $ \pid -> do
    bird <- stage Enemies.carrionBird
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource ally 1
    getEngagementCost bird `shouldReturn` 39
    run $ EngageEnemy pid bird
    getsGame (length . (.entities.allies)) `shouldReturn` 0

  -- \* Locations

  it "gives Burnt Sands +2 threat at 40 or higher, and raises the temperature by 2 as it becomes active" . desertTest $ \_ -> do
    setTemperature 38
    sands <- stageLocation Locations.burntSands
    getLocationThreat sands `shouldReturn` 2
    run $ TravelTo sands
    getTemperature `shouldReturn` 40
    getLocationThreat sands `shouldReturn` 4

  it "has the first player assign X damage among characters as Scorched Hamada becomes active" . desertTest $ \pid -> do
    setTemperature 20
    hamada <- stageLocation Locations.scorchedHamada
    run $ TravelTo hamada
    beravor <- hero "01012"
    replicateM_ 2 $ chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2

  it "raises the defending player's threat by X for Scorched Hamada's shadow" . desertTest $ \pid -> do
    setTemperature 30
    troll <- stage Enemies.hillTroll
    run $ EngageEnemy pid troll
    threat0 <- (.threat) <$> getPlayer pid
    resolveShadow pid troll Locations.scorchedHamada
    (.threat) <$> getPlayer pid `shouldReturn` (threat0 + 3)

  it "raises Creature enemies while Parched Wadi is staged, and adds a Creature from the shuffled deck as it becomes active" . desertTest $ \_ -> do
    wadi <- stageLocation Locations.parchedWadi
    scorpion <- stage Enemies.giantScorpion
    getEnemyAttack scorpion `shouldReturn` 4
    viper <- genEncounterCard Enemies.sandViper
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [viper])
    _ <- setEncounterDeck []
    run $ TravelTo wadi
    sort <$> enemyCodes `shouldReturn` sort (map toCardCode [Enemies.giantScorpion, Enemies.sandViper])

  -- \* Treacheries

  it "discards the active location for Mirage and makes the next location discarded active" . desertTest $ \_ -> do
    sands <- stageLocation Locations.burntSands
    run $ TravelTo sands
    setTemperature 10
    reveal [Treacheries.mirage, Treacheries.unbearableHeat, Locations.scorchedHamada]
    -- the Burnt Sands discarded is shuffled back in, and may be the location found
    ls <- locationsIn
    map snd ls `shouldBe` [ActiveLocation]
    map fst ls `shouldSatisfy` all (`elem` map toCardCode [Locations.burntSands, Locations.scorchedHamada])

  it "has each player assign X damage for Unbearable Heat" . desertTest $ \pid -> do
    setTemperature 20
    reveal [Treacheries.unbearableHeat]
    getQuestion pid >>= (`shouldSatisfy` isQuestion "$label.dealDamageDivided")

  it "raises the temperature by 4 for Scorching Sun, and surges" . desertTest $ \_ -> do
    setTemperature 10
    reveal [Treacheries.scorchingSun, Locations.burntSands]
    getTemperature `shouldReturn` 14
    map fst <$> locationsIn `shouldReturn` [toCardCode Locations.burntSands]

  it "raises the temperature by 2 when the player chooses so for Scorching Sun's shadow" . desertTest $ \pid -> do
    setTemperature 10
    troll <- stage Enemies.hillTroll
    run $ EngageEnemy pid troll
    resolveShadow pid troll Treacheries.scorchingSun
    chooseLabel "$label.scorchingSun.temperature" pid
    getTemperature `shouldReturn` 12
