module LordOfTheRings.DreamChaser.FlightOfTheStormcallerSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Heading (Heading (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (..))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (flightOfTheStormcaller, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (Meta (..), getStormcallerMeta)
import LordOfTheRings.Scenario.Types (headingL, metaL, questDeckL, setAsideL)
import LordOfTheRings.Window (WindowType (PhaseEnds, RoundEnds), mkWhen)
import TestImport

stormTest :: (PlayerId -> GameT ()) -> IO ()
stormTest = gameTestIn flightOfTheStormcaller testDeck

refresh :: Game -> GameT ()
refresh g = putGame g >> (putGame =<< preloadModifiers g)

setHeading :: Heading -> GameT ()
setHeading h = refresh . (scenarioL %~ overAttrs (headingL ?~ h)) =<< getGame

-- | The Stormcaller's area prepared, the Stormcaller in it, its stage the one given with
-- the stages after it beneath and this much progress on it.
prepareArea :: CardDef -> [CardDef] -> Int -> GameT EnemyId
prepareArea stageDef rest progress = do
  stormcaller <- genEncounterCard Enemies.stormcaller
  getGame >>= putGame . (scenarioL %~ overAttrs (setAsideL .~ [stormcaller]))
  run $ ScenarioSpecific "stormcaller.prepare" Null
  stage <- genEncounterCard stageDef
  deck <- traverse genEncounterCard rest
  refresh . (scenarioL %~ overAttrs (metaL .~ toJSON (Meta (Just stage) deck progress))) =<< getGame
  enemyOf Enemies.stormcaller

-- | A Ship enemy engaged with the player attacks undefended with Sahír's Getting Away! as its
-- shadow, its damage going to the player's Ship objective, which may be kept from taking it.
sahirsAttack :: PlayerId -> Bool -> GameT Target
sahirsAttack pid shielded = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.dreamChaser
  ship <- getsGame (toTarget . fromJustNote "no ship" . find ((== toCardCode Objectives.dreamChaser) . toCardCode) . toList . (.entities.allies))
  when shielded $ run $ CreateEffect $ Effect GameSource ship [CannotTakeDamage] UntilEndOfPhase
  run . PutEnemyIntoPlayEngaged pid =<< genEncounterCard Enemies.patrolShip
  eid <- enemyOf Enemies.patrolShip
  _ <- setEncounterDeck [Treacheries.sahirsGettingAway]
  run $ DealShadowCard eid
  run $ EnemyAttack pid eid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.undefendedDamage" _ _ _) -> chooseTarget pid ship
    _ -> pure ()
  passActions pid
  pure ship

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOf :: EnemyId -> GameT Placement
placementOf eid = getsGame (maybe Unplaced (.placement) . find ((== eid) . toId) . toList . (.entities.enemies))

stormStage :: GameT (Maybe CardCode)
stormStage = fmap toCardCode . (.stormcallerStage) <$> getStormcallerMeta

stormProgress :: GameT Int
stormProgress = (.stormcallerProgress) <$> getStormcallerMeta

-- | The stage in play on its B side, the stages given beneath it.
flippedStage :: CardDef -> [CardDef] -> GameT QuestId
flippedStage def rest = do
  QuestTarget q <- putQuestStage def
  deck <- traverse genEncounterCard rest
  refresh . (scenarioL %~ overAttrs (questDeckL .~ deck)) . (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) =<< getGame
  pure q

questProgress :: GameT (Maybe Int)
questProgress = getsGame (fmap (.progress) . headMay . toList . (.entities.quests))

endRound :: GameT ()
endRound = run $ CheckWindows [mkWhen RoundEnds]

spec :: Spec
spec = describe "Flight of the Stormcaller" do
  it "is a scenario the game can be dealt" . stormTest $ \_ ->
    isJust (lookupScenario "flight-of-the-stormcaller") `shouldBe` True

  it "Setup prepares the Corsair deck, the fleet and the Stormcaller's area, and stages Rolling Seas" . stormTest $ \pid -> do
    flipFirstQuest pid
    chooseOptionMatching "choose a ship" pid (const True)
    stormcaller <- enemyOf Enemies.stormcaller
    placementOf stormcaller `shouldReturn` StormcallerArea
    stormStage `shouldReturn` Just (toCardCode Quests.fullSailAheadStormcaller)
    map toCardCode . (.stormcallerDeck) <$> getStormcallerMeta
      `shouldReturn` map toCardCode [Quests.theWeatherTurnsFoulStormcaller, Quests.theCapeOfAndrastStormcaller]
    getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario))
      `shouldReturn` map toCardCode [Quests.fullSailAhead, Quests.theWeatherTurnsFoul, Quests.theCapeOfAndrast]
    getsGame (map toCardCode . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
      `shouldReturn` [toCardCode Locations.rollingSeas]
    deck <- getEncounterDeck
    corsairs <- getsGame (fromMaybe [] . (.corsairDeck) . toAttrs . (.scenario))
    map toCardCode (deck <> corsairs) `shouldSatisfy` notElem (toCardCode Enemies.stormcaller)
    map toCardCode corsairs `shouldSatisfy` elem (toCardCode Enemies.serpentsBlade)

  describe "The Stormcaller's area" do
    it "Sahír's Getting Away!'s shadow places progress once the attack damages a Ship objective" . stormTest $ \pid -> do
      _ <- prepareArea Quests.fullSailAheadStormcaller [] 0
      ship <- sahirsAttack pid False
      stormProgress `shouldReturn` 1
      getCharacterDamage ship >>= (`shouldSatisfy` (> 0))

    it "Sahír's Getting Away!'s shadow places none when the Ship objective takes no damage" . stormTest $ \pid -> do
      _ <- prepareArea Quests.fullSailAheadStormcaller [] 0
      _ <- sahirsAttack pid True
      stormProgress `shouldReturn` 0

    it "keeps its cards out of the staging area and immune to player card effects" . stormTest $ \_ -> do
      stormcaller <- prepareArea Quests.fullSailAheadStormcaller [] 0
      getModifiers (EnemyTarget stormcaller) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getStagingThreat `shouldReturn` 0

    it "has the Stormcaller place its threat plus the discarded card's at the end of the round, +2 off-course" . stormTest $ \_ -> do
      _ <- prepareArea Quests.fullSailAheadStormcaller [Quests.theWeatherTurnsFoulStormcaller] 0
      setHeading Sunny
      _ <- setEncounterDeck [Locations.hiddenCove]
      endRound
      stormProgress `shouldReturn` 5
      setHeading Cloudy
      _ <- setEncounterDeck [Locations.hiddenCove]
      endRound
      -- 5 and then 4 + 3 make the 12 stage 2C takes
      stormProgress `shouldReturn` 0
      stormStage `shouldReturn` Just (toCardCode Quests.theWeatherTurnsFoulStormcaller)

    it "counts Sahír's Escort in its area, where its When Revealed puts it" . stormTest $ \_ -> do
      _ <- prepareArea Quests.fullSailAheadStormcaller [] 0
      setHeading Sunny
      _ <- setEncounterDeck [Enemies.sahirsEscort]
      run RevealEncounterCard
      resolveRevealed
      escort <- enemyOf Enemies.sahirsEscort
      placementOf escort `shouldReturn` StormcallerArea
      _ <- setEncounterDeck [Locations.hiddenCove]
      endRound
      stormProgress `shouldReturn` 7

    it "advances the Stormcaller, which returns to its area, and stage 3C brings Sahír's Escort" . stormTest $ \pid -> do
      stormcaller <- prepareArea Quests.fullSailAheadStormcaller [Quests.theWeatherTurnsFoulStormcaller, Quests.theCapeOfAndrastStormcaller] 10
      run $ PlaceEnemy stormcaller (EngagedWith pid)
      setHeading Sunny
      _ <- setEncounterDeck [Locations.hiddenCove, Enemies.sahirsEscort]
      endRound
      stormStage `shouldReturn` Just (toCardCode Quests.theWeatherTurnsFoulStormcaller)
      stormProgress `shouldReturn` 0
      placementOf stormcaller `shouldReturn` StormcallerArea
      escort <- enemyOf Enemies.sahirsEscort
      placementOf escort `shouldReturn` StormcallerArea

    it "allows no more than 4 damage on the Stormcaller each round at stage 2C" . stormTest $ \_ -> do
      stormcaller <- prepareArea Quests.fullSailAheadStormcaller [] 0
      run $ PlaceTokens GameSource (EnemyTarget stormcaller) Damage 6
      getDamage (EnemyTarget stormcaller) `shouldReturn` 4

  describe "Stage matching" do
    it "puts the Stormcaller's area in the staging area once the players reach its stage" . stormTest $ \pid -> do
      stormcaller <- prepareArea Quests.fullSailAheadStormcaller [] 0
      departure <- flippedStage Quests.swiftDeparture [Quests.fullSailAhead, Quests.theWeatherTurnsFoul]
      run $ CompleteQuest departure
      -- Full Sail Ahead!'s search finds no Swift Raider
      resolveRevealed
      chooseLabel "$label.continue" pid
      placementOf stormcaller `shouldReturn` StagingArea

    it "Swift Departure moves 2 progress to the Stormcaller's stage at the end of the quest phase while off-course" . stormTest $ \_ -> do
      _ <- prepareArea Quests.fullSailAheadStormcaller [] 0
      departure <- flippedStage Quests.swiftDeparture []
      run $ PlaceTokens GameSource (QuestTarget departure) Progress 3
      setHeading Cloudy
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      questProgress `shouldReturn` Just 1
      stormProgress `shouldReturn` 2

    it "Full Sail Ahead! takes no more than 4 progress a round while off-course" . stormTest $ \_ -> do
      _ <- prepareArea Quests.fullSailAheadStormcaller [] 0
      q <- flippedStage Quests.fullSailAhead []
      setHeading Cloudy
      run $ PlaceTokens GameSource (QuestTarget q) Progress 10
      questProgress `shouldReturn` Just 4

  describe "Winning and losing" do
    it "the players win defeating the stage the Stormcaller is at" . stormTest $ \_ -> do
      _ <- prepareArea Quests.fullSailAheadStormcaller [] 0
      q <- flippedStage Quests.fullSailAhead [Quests.theWeatherTurnsFoul, Quests.theCapeOfAndrast]
      run $ CompleteQuest q
      getsGame (.outcome) `shouldReturn` Just Won

    it "the players advance defeating a stage the Stormcaller is past" . stormTest $ \_ -> do
      _ <- prepareArea Quests.theWeatherTurnsFoulStormcaller [] 0
      q <- flippedStage Quests.fullSailAhead [Quests.theWeatherTurnsFoul, Quests.theCapeOfAndrast]
      run $ CompleteQuest q
      getsGame (.outcome) `shouldReturn` Nothing
      getsGame (map toCardCode . toList . (.entities.quests)) `shouldReturn` [toCardCode Quests.theWeatherTurnsFoul]

    it "the players win as the Stormcaller leaves play" . stormTest $ \_ -> do
      stormcaller <- prepareArea Quests.fullSailAheadStormcaller [] 0
      run $ Discard GameSource (EnemyTarget stormcaller)
      getsGame (.outcome) `shouldReturn` Just Won

    it "the players lose as the Stormcaller defeats stage 4C" . stormTest $ \_ -> do
      _ <- prepareArea Quests.theCapeOfAndrastStormcaller [] 22
      setHeading Sunny
      _ <- setEncounterDeck [Locations.hiddenCove]
      endRound
      getsGame (.outcome) `shouldReturn` Just Lost
