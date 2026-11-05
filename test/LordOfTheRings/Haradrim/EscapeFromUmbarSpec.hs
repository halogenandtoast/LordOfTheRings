module LordOfTheRings.Haradrim.EscapeFromUmbarSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost, getLocationThreat)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (escapeFromUmbar, lookupScenario)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

umbarTest :: (PlayerId -> GameT ()) -> IO ()
umbarTest = gameTestIn escapeFromUmbar testDeck

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

enemiesOf :: CardDef -> GameT [Placement]
enemiesOf def = getsGame (map (.placement) . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationAt :: LocationId -> GameT (Maybe LocationAttrs)
locationAt lid = getsGame (fmap toAttrs . lookup lid . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

activate :: CardDef -> GameT LocationId
activate def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

-- | The stage in play, on its B side, its When Revealed passed by.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  pure q

mainProgress :: GameT Int
mainProgress = getsGame (sum . map (.progress) . toList . (.entities.quests))

questsInPlay :: GameT [CardCode]
questsInPlay = getsGame (map toCardCode . toList . (.entities.quests))

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

chooseAnyTarget :: HasCallStack => PlayerId -> GameT ()
chooseAnyTarget pid = chooseOptionMatching "choose a card" pid \case
  TargetLabel {} -> True
  _ -> False

hero :: GameT Target
hero = toTarget <$> getHeroWithCode "01012"

spec :: Spec
spec = describe "Escape from Umbar" do
  it "is a scenario the game can be dealt" . umbarTest $ \_ ->
    isJust (lookupScenario "escape-from-umbar") `shouldBe` True

  it "adds Seize Them! and a location to the staging area and engages each player with an Umbar Sentry" . umbarTest $ \pid -> do
    flipFirstQuest pid
    chooseAnyTarget pid
    objectives <- getsGame (map (\o -> (toCardCode o, o.placement)) . toList . (.entities.objectives))
    objectives `shouldBe` [(toCardCode Objectives.seizeThem, StagingArea)]
    getsGame (map (.placement) . toList . (.entities.locations)) `shouldReturn` [StagingArea]
    enemiesOf Enemies.umbarSentry `shouldReturn` [EngagedWith pid]
    q <- getActiveQuest
    (toCardCode q, (toAttrs q).flipped) `shouldBe` (toCardCode Quests.strandedInUmbar, True)
    deck <- getEncounterDeck
    any ((`elem` [toCardCode Objectives.seizeThem]) . toCardCode) deck `shouldBe` False
    -- 4 Umbar Sentries in easy mode or 5 normally, one of them in play
    length (filter ((== toCardCode Enemies.umbarSentry) . toCardCode) deck) `shouldBe` 4

  it "advances from Stranded in Umbar to Flee the City once its 15 progress is placed and no one is engaged" . umbarTest $ \pid -> do
    flipFirstQuest pid
    chooseAnyTarget pid
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 15
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.strandedInUmbar
    sentry <- enemyOf Enemies.umbarSentry
    run $ Discard GameSource (toTarget sentry)
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.fleeTheCity
    resolveWhenRevealed pid
    getsGame (any ((== EngagedWith pid) . (.placement)) . toList . (.entities.enemies)) `shouldReturn` True

  it "caps Stranded in Umbar at 15 progress, undefeated while a player is engaged" . gameTest $ \pid -> do
    q <- flippedStage Quests.strandedInUmbar
    sentry <- engaged pid Enemies.umbarSentry
    run $ PlaceTokens GameSource q Progress 20
    mainProgress `shouldReturn` 15
    questsInPlay `shouldReturn` [toCardCode Quests.strandedInUmbar]
    run $ Discard GameSource (toTarget sentry)
    questsInPlay `shouldReturn` []

  it "has Flee the City end the phase and engage each player with the first enemy he discards" . gameTest $ \pid -> do
    _ <- setEncounterDeck [Locations.umbarSquare, Enemies.umbarSentry]
    run . AdvanceToQuest =<< genEncounterCard Quests.fleeTheCity
    resolveWhenRevealed pid
    -- the deck is shuffled first, so the location may or may not be discarded first
    enemiesOf Enemies.umbarSentry `shouldReturn` [EngagedWith pid]
    getsGame (length . (.entities.locations)) `shouldReturn` 0

  it "has Flee the City add an enemy to the staging area as the quest phase begins, if a player is not engaged" . gameTest $ \_ -> do
    _ <- flippedStage Quests.fleeTheCity
    _ <- setEncounterDeck [Treacheries.enemyPursuit, Enemies.umbarSentry]
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    enemiesOf Enemies.umbarSentry `shouldReturn` [StagingArea]

  describe "Seize Them!" do
    let seized = run . AddToStaging =<< genEncounterCard Objectives.seizeThem
        beginRound = do
          runOnly BeginRound
          withQueue_ (const [])
        endRound = run $ CheckWindows [mkWhen RoundEnds]

    it "loses the game at the end of a round the main quest spent in play with no progress" . umbarTest $ \_ -> do
      seized
      _ <- flippedStage Quests.strandedInUmbar
      beginRound
      endRound
      getsGame (.outcome) `shouldReturn` Just Lost

    it "does not lose with progress on the main quest" . umbarTest $ \_ -> do
      seized
      q <- flippedStage Quests.strandedInUmbar
      beginRound
      run $ PlaceTokens GameSource q Progress 1
      endRound
      getsGame (.outcome) `shouldReturn` Nothing

    it "does not lose the round a new main quest comes into play" . umbarTest $ \_ -> do
      seized
      beginRound
      _ <- flippedStage Quests.fleeTheCity
      endRound
      getsGame (.outcome) `shouldReturn` Nothing

    it "removes the attacking enemy's threat in progress as its attack destroys a character" . gameTest $ \pid -> do
      seized
      q <- flippedStage Quests.fleeTheCity
      run $ PlaceTokens GameSource q Progress 5
      soldier <- stage Enemies.southronSoldier
      h <- hero
      run $ CheckWindows [mkAfter $ CharacterDestroyedByAttack soldier h pid]
      mainProgress `shouldReturn` 2

  it "has Umbar Sentry remove 1 progress from the main quest or get +2 attack as it attacks" . gameTest $ \pid -> do
    q <- flippedStage Quests.fleeTheCity
    run $ PlaceTokens GameSource q Progress 2
    sentry <- engaged pid Enemies.umbarSentry
    run $ CheckWindows [mkWhen $ EnemyAttacking sentry pid]
    chooseLabelStarting "$label.removeMainQuestProgress" pid
    mainProgress `shouldReturn` 1
    getEnemyAttack sentry `shouldReturn` 2
    run $ CheckWindows [mkWhen $ EnemyAttacking sentry pid]
    chooseLabel "$label.umbarSentry.attack" pid
    getEnemyAttack sentry `shouldReturn` 4

  it "has Narrow Alleyway hide an engaged enemy out of play until it leaves play" . gameTest $ \pid -> do
    lid <- activate Locations.narrowAlleyway
    sentry <- engaged pid Enemies.umbarSentry
    run $ CheckWindows [mkAfter $ Traveled lid]
    chooseAbilityOf pid lid
    placementOfEnemy sentry `shouldReturn` Nothing
    fmap (length . locationUnderneath) <$> locationAt lid `shouldReturn` Just 1
    run $ Discard GameSource (toTarget lid)
    enemiesOf Enemies.umbarSentry `shouldReturn` [EngagedWith pid]

  it "has Umbar Street lower each enemy's engagement cost by the main quest's progress" . gameTest $ \_ -> do
    q <- flippedStage Quests.fleeTheCity
    run $ PlaceTokens GameSource q Progress 3
    sentry <- stage Enemies.umbarSentry
    _ <- activate Locations.umbarStreet
    getEngagementCost sentry `shouldReturn` 25

  it "has Umbar Square place progress on the main quest as an enemy is defeated" . gameTest $ \pid -> do
    _ <- flippedStage Quests.fleeTheCity
    lid <- activate Locations.umbarSquare
    card <- genEncounterCard Enemies.umbarSentry
    run $ CheckWindows [mkAfter $ EnemyDestroyed (EncounterCard card)]
    chooseAbilityOf pid lid
    mainProgress `shouldReturn` 1

  it "has Umbar Square reveal the top encounter card to travel there" . gameTest $ \_ -> do
    lid <- stageLocation Locations.umbarSquare
    _ <- setEncounterDeck [Enemies.umbarSentry]
    run $ TravelTo lid
    enemiesOf Enemies.umbarSentry `shouldReturn` [StagingArea]

  it "gives Port of Harad 1 threat for each engaged enemy and that much damage to travel there" . gameTest $ \pid -> do
    lid <- stageLocation Locations.portOfHarad
    _ <- engaged pid Enemies.umbarSentry
    _ <- engaged pid Enemies.umbarSentry
    getLocationThreat lid `shouldReturn` 2
    run $ TravelTo lid
    h <- hero
    chooseTarget pid h
    chooseTarget pid h
    getDamage h `shouldReturn` 2

  it "takes no progress on Haradrim Estate in the staging area, and engages a Harad enemy to travel there" . gameTest $ \pid -> do
    _ <- flippedStage Quests.fleeTheCity
    lid <- stageLocation Locations.haradrimEstate
    -- a player card places it, as only player cards place progress there
    source <- toSource <$> getHeroWithCode "01012"
    run $ PlaceTokens source (toTarget lid) Progress 1
    fmap (.progress) <$> locationAt lid `shouldReturn` Just 0
    _ <- setEncounterDeck [Locations.umbarSquare, Enemies.umbarSentry]
    run $ TravelTo lid
    chooseAnyTarget pid
    enemiesOf Enemies.umbarSentry `shouldReturn` [EngagedWith pid]
    mainProgress `shouldReturn` 1

  it "has Enemy Pursuit remove all progress from the main quest, or deal that much damage" . gameTest $ \pid -> do
    q <- flippedStage Quests.fleeTheCity
    run $ PlaceTokens GameSource q Progress 3
    _ <- setEncounterDeck [Treacheries.enemyPursuit]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.enemyPursuit.removeProgress" pid
    mainProgress `shouldReturn` 0
