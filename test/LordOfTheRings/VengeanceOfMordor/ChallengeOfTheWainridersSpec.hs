module LordOfTheRings.VengeanceOfMordor.ChallengeOfTheWainridersSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (..), CardType (LocationType), RacingResult (..), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Indestructible, Race, Surge))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (..), PlayerMatcher (..), getEnemy, select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (challengeOfTheWainriders, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (Circuit (..), getRace)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

raceTest :: (PlayerId -> GameT ()) -> IO ()
raceTest = gameTestIn challengeOfTheWainriders testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

beravor, eowyn, theodred :: GameT Target
beravor = hero "01012"
eowyn = hero "01007"
theodred = hero "01002"

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

attachedTo :: EnemyId -> GameT [CardCode]
attachedTo eid = getsGame (map toCardCode . filter ((== AttachedTo (EnemyTarget eid)) . (.placement)) . toList . (.entities.treacheries))

objectivePlacements :: GameT [(CardCode, Placement)]
objectivePlacements = getsGame (sort . map (\o -> (toCardCode o, (toAttrs o).placement)) . toList . (.entities.objectives))

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

activeProgress :: GameT Int
activeProgress = (.progress) . toAttrs <$> getActiveQuest

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | Set The Challengers and The Wainriders where they are, and the damage on The
-- Wainriders' stage, the current quest the stage The Challengers is at, at the start of a
-- round.
setRace :: Int -> Int -> Int -> GameT ()
setRace challengers wainriders damage = do
  r <- getRace
  g <- getGame
  let stageCard = fromJustNote "no stage" $ headMay $ drop (challengers `mod` 6) r.circuit
  putGame $ g & entitiesL . questsL .~ mempty & scenarioL %~ overAttrs (\a -> a {scenarioMeta = toJSON r {challengers, wainriders, wainridersDamage = damage, completedThisRound = 0}})
  run $ PutQuestIntoPlay stageCard
  run $ Flip (CardIdTarget stageCard.id)
  putGame =<< preloadModifiers =<< getGame

-- | Setup, alone at the table: Wainrider Champion in the staging area.
setUp :: PlayerId -> GameT ()
setUp = flipFirstQuest

-- | Progress placed on the current quest during the quest phase, as questing places it.
questProgress :: Int -> GameT ()
questProgress n = do
  inPhase QuestPhase
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress n

spec :: Spec
spec = describe "Challenge of the Wainriders" do
  it "is a scenario the game can be dealt" . raceTest $ \_ ->
    isJust (lookupScenario "challenge-of-the-wainriders") `shouldBe` True

  -- \* Setup

  it "builds the circuit, places The Challengers and The Wainriders at stage 1B, and adds Wainrider Champion to the staging area" . raceTest $ \pid -> do
    setUp pid
    attrs <- scenarioAttrs
    let sets = ordNub $ mapMaybe (cdEncounterSet . toCardDef) attrs.encounterDeck
    sort sets `shouldBe` sort [ChallengeOfTheWainriders, RollingPlains, RidersOfRhun]
    -- 25 cards less The Wainriders, The Challengers and Wainrider Champion; 6 and 11
    length attrs.encounterDeck `shouldBe` 22 + 6 + 11
    enemyCodes `shouldReturn` [toCardCode Enemies.wainriderChampion]
    objectivePlacements `shouldReturn` sort [(toCardCode Objectives.theWainriders, Unplaced), (toCardCode Objectives.theChallengers, Unplaced)]
    r <- getRace
    map toCardCode r.circuit
      `shouldBe` map toCardCode [Quests.wainriderCamp, Quests.firstSprint, Quests.theTurnaround, Quests.theFarthestEdge, Quests.lastClimb, Quests.homeStretch]
    (r.challengers, r.wainriders, r.wainridersDamage) `shouldBe` (0, 0, 0)
    activeQuestCode `shouldReturn` toCardCode Quests.wainriderCamp
    (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

  it "has each other player add a location of a different title to the staging area" . twoPlayerTestIn challengeOfTheWainriders testDeck $ \one two -> do
    run Setup
    replicateM_ 2 do
      asked <- filterM (fmap isJust . getQuestion) [one, two]
      for_ (take 1 asked) $ chooseLabel "$label.keepHand"
    lead <- getLead
    let other = if lead == one then two else one
    chooseOptionMatching "flip the quest" lead (const True)
    -- the other player searches the encounter deck for a location
    chooseOptionMatching "choose a location" other \case
      TargetLabel (CardIdTarget _) _ -> True
      _ -> False
    locations <- getsGame (map toCardDef . toList . (.entities.locations))
    map cdCardType locations `shouldBe` [LocationType]
    enemyCodes `shouldReturn` [toCardCode Enemies.wainriderChampion]
    (.questFlipped) . toAttrs <$> getActiveQuest `shouldReturn` True

  it "keeps fewer copies in easy mode" . raceTest $ \_ -> do
    map cdEasyModeQuantity [Enemies.aggressiveRacer, Enemies.chariotDriver, Enemies.veteranWainrider, Locations.narrowPass, Locations.crowdedTrack, Locations.ravineLedge, Treacheries.wainriderChariot, Treacheries.theChampionsCunning]
      `shouldBe` [Just 3, Just 2, Nothing, Nothing, Just 1, Just 1, Just 1, Just 2]

  it "prints a racing result on each card of the encounter set" . raceTest $ \_ -> do
    map cdRacingResult [Enemies.aggressiveRacer, Enemies.chariotDriver, Enemies.veteranWainrider, Locations.narrowPass, Locations.crowdedTrack, Locations.ravineLedge, Treacheries.wainriderChariot, Treacheries.theChampionsCunning, Enemies.wainriderChampion]
      `shouldBe` [Just Turn, Just Straight, Just Straight, Just Straight, Just Straight, Just Turn, Just Straight, Just Wild, Nothing]
    all ((== setFromList [Race]) . cdKeywords) [Quests.wainriderCamp, Quests.homeStretch] `shouldBe` True

  -- \* Race keyword and racing tests

  it "gives each stage 4 quest points per player, and takes no progress from player card effects" . raceTest $ \pid -> do
    setUp pid
    q <- toTarget <$> getActiveQuest
    getModifiers q >>= (`shouldSatisfy` elem (QuestPointModifier 4))
    b <- toId <$> getHeroWithCode "01012"
    run $ PlaceTokens (HeroSource b) q Progress 2
    activeProgress `shouldReturn` 0

  it "cannot be defeated outside of the quest phase" . raceTest $ \pid -> do
    setUp pid
    inPhase CombatPhase
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 6
    activeQuestCode `shouldReturn` toCardCode Quests.wainriderCamp
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "passes stage 1B's racing Defense test with a STRAIGHT result: The Challengers advance, the excess progress placed on the next stage" . raceTest $ \pid -> do
    setUp pid
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Enemies.chariotDriver, Locations.narrowPass]
    questProgress 6
    -- Beravor's 2 defense: the top 2 cards, a TURN and a STRAIGHT
    chooseTarget pid =<< beravor
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    length <$> getEncounterDeck `shouldReturn` 1
    activeQuestCode `shouldReturn` toCardCode Quests.firstSprint
    activeProgress `shouldReturn` 2
    r <- getRace
    (r.challengers, r.completedThisRound) `shouldBe` (1, 1)

  it "requires an additional result for each stage completed this round, and starts again next round" . raceTest $ \pid -> do
    setUp pid
    setRace 1 0 0
    r0 <- getRace
    let g = r0 {completedThisRound = 1}
    putGame . (scenarioL %~ overAttrs (\a -> a {scenarioMeta = toJSON g})) =<< getGame
    _ <- setEncounterDeck [Enemies.chariotDriver, Enemies.aggressiveRacer, Locations.narrowPass]
    questProgress 4
    -- Beravor's 2 attack: 1 STRAIGHT where 2 are required, so each character takes 1 damage
    chooseTarget pid =<< beravor
    (traverse getDamage =<< sequence [beravor, eowyn, theodred]) `shouldReturn` [1, 1, 1]
    -- still defeated: The Challengers advance, with no excess progress
    activeQuestCode `shouldReturn` toCardCode Quests.theTurnaround
    activeProgress `shouldReturn` 0
    run EndRound
    (.completedThisRound) <$> getRace `shouldReturn` 0

  -- \* The stages' failed racing tests

  it "has each player discard 1 random card from his hand when stage 1B's test fails" . raceTest $ \pid -> do
    setUp pid
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Enemies.aggressiveRacer, Locations.narrowPass]
    cards <- handSize pid
    questProgress 4
    chooseTarget pid =<< beravor
    handSize pid `shouldReturn` cards - 1
    activeQuestCode `shouldReturn` toCardCode Quests.firstSprint

  it "has each player discard an ally he controls when stage 3B's racing Willpower test, needing a TURN, fails" . raceTest $ \pid -> do
    setUp pid
    setRace 2 0 0
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Enemies.chariotDriver, Enemies.aggressiveRacer]
    questProgress 4
    -- Théodred's 1 willpower: a STRAIGHT, no TURN
    chooseTarget pid =<< theodred
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    activeQuestCode `shouldReturn` toCardCode Quests.theFarthestEdge

  it "counts a WILD result as a TURN" . raceTest $ \pid -> do
    setUp pid
    setRace 2 0 0
    _ <- setEncounterDeck [Treacheries.theChampionsCunning, Locations.narrowPass]
    questProgress 5
    chooseTarget pid =<< theodred
    activeQuestCode `shouldReturn` toCardCode Quests.theFarthestEdge
    activeProgress `shouldReturn` 1

  it "reveals the top card of the encounter deck when stage 4B's test fails" . raceTest $ \pid -> do
    setUp pid
    setRace 3 0 0
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Locations.narrowPass]
    questProgress 4
    chooseTarget pid =<< theodred
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.narrowPass]
    activeQuestCode `shouldReturn` toCardCode Quests.lastClimb

  it "places 2 damage per player on The Wainriders' stage when stage 5B's test fails" . raceTest $ \pid -> do
    setUp pid
    setRace 4 0 0
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Locations.narrowPass]
    questProgress 4
    chooseTarget pid =<< theodred
    (.wainridersDamage) <$> getRace `shouldReturn` 2
    activeQuestCode `shouldReturn` toCardCode Quests.homeStretch

  it "leaves stage 6B undefeated, its progress discarded, when its test fails" . raceTest $ \pid -> do
    setUp pid
    setRace 5 0 0
    _ <- setEncounterDeck [Enemies.chariotDriver, Locations.narrowPass]
    questProgress 5
    chooseTarget pid =<< eowyn
    activeQuestCode `shouldReturn` toCardCode Quests.homeStretch
    activeProgress `shouldReturn` 0
    (.challengers) <$> getRace `shouldReturn` 5

  it "completes the circuit at stage 6B, and wins as The Challengers complete it twice" . raceTest $ \pid -> do
    setUp pid
    setRace 5 0 0
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Locations.narrowPass, Enemies.aggressiveRacer]
    questProgress 4
    chooseTarget pid =<< eowyn
    activeQuestCode `shouldReturn` toCardCode Quests.wainriderCamp
    setRace 11 6 0
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Locations.narrowPass]
    questProgress 4
    chooseTarget pid =<< theodred
    outcome `shouldReturn` Just Won

  -- \* The Wainriders

  it "places damage equal to the staging area's threat on The Wainriders' stage at the end of the quest phase, advancing them with the excess" . raceTest $ \pid -> do
    setUp pid
    _ <- stage Enemies.chariotDriver
    -- Wainrider Champion's 2 and Chariot Driver's 3, against 4 quest points
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    r <- getRace
    (r.wainriders, r.wainridersDamage) `shouldBe` (1, 1)

  it "loses as The Wainriders complete the circuit twice" . raceTest $ \pid -> do
    setUp pid
    setRace 3 11 3
    _ <- stage Enemies.aggressiveRacer
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    outcome `shouldReturn` Just Lost

  -- \* Ravine Ledge

  it "requires 1 additional result with Ravine Ledge in the staging area" . raceTest $ \pid -> do
    setUp pid
    _ <- stageLocation Locations.ravineLedge
    cards <- handSize pid
    _ <- setEncounterDeck [Enemies.chariotDriver, Enemies.aggressiveRacer, Locations.narrowPass]
    questProgress 4
    chooseTarget pid =<< beravor
    handSize pid `shouldReturn` cards - 1

  it "has the first player make a racing Attack test traveling to Ravine Ledge, its own additional result required" . raceTest $ \pid -> do
    setUp pid
    ledge <- stageLocation Locations.ravineLedge
    cards <- handSize pid
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Treacheries.theChampionsCunning, Locations.narrowPass]
    run $ TravelTo ledge
    chooseTarget pid =<< beravor
    handSize pid `shouldReturn` cards + 1
    run $ Discard GameSource (toTarget ledge)
    ledge2 <- stageLocation Locations.ravineLedge
    _ <- setEncounterDeck [Enemies.aggressiveRacer, Locations.narrowPass, Locations.narrowPass]
    run $ TravelTo ledge2
    chooseTarget pid =<< theodred
    handSize pid `shouldReturn` cards

  -- \* Wainrider Champion

  it "gives Wainrider Champion 2 Threat and Defense per player, engaged with each player while The Challengers are at The Wainriders' stage" . raceTest $ \pid -> do
    setUp pid
    champion <- enemyOf Enemies.wainriderChampion
    getEnemyThreat champion `shouldReturn` 2
    getEnemyDefense champion `shouldReturn` 2
    mods <- getModifiers champion
    mods `shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, EngagedWithEachPlayer AnyPlayer]
    cdKeywords Enemies.wainriderChampion `shouldBe` setFromList [Indestructible]
    select (EnemyEngagedWith $ PlayerWithId pid) `shouldReturn` [champion]
    setRace 1 0 0
    getModifiers champion >>= (`shouldSatisfy` notElem (EngagedWithEachPlayer AnyPlayer))

  -- \* Encounter cards

  it "gives Aggressive Racer surge while The Challengers are ahead, and places 2 damage after it destroys a character" . raceTest $ \pid -> do
    setUp pid
    racer <- stage Enemies.aggressiveRacer
    getModifiers racer >>= (`shouldSatisfy` notElem (GainKeyword Surge))
    setRace 1 0 0
    getModifiers racer >>= (`shouldSatisfy` elem (GainKeyword Surge))
    run $ EngageEnemy pid racer
    guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    shadowAttack pid racer Locations.narrowPass guardian
    (.wainridersDamage) <$> getRace `shouldReturn` 2

  it "has Chariot Driver place 3 damage on The Wainriders' stage or attack the first player, and return to the staging area with its shadow" . raceTest $ \pid -> do
    setUp pid
    _ <- setEncounterDeck [Enemies.chariotDriver]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.chariotDriver.damage" pid
    (.wainridersDamage) <$> getRace `shouldReturn` 3
    -- The Challengers ahead, Wainrider Champion engaged with no one
    setRace 1 0 0
    racer <- stage Enemies.aggressiveRacer
    run $ EngageEnemy pid racer
    guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    shadowAttack pid racer Enemies.chariotDriver guardian
    (.placement) <$> getEnemy racer `shouldReturn` StagingArea

  it "gives Veteran Wainrider +2 Threat and keeps it in the staging area while The Challengers are ahead, and +2 Attack with its shadow" . raceTest $ \pid -> do
    setUp pid
    veteran <- stage Enemies.veteranWainrider
    getEnemyThreat veteran `shouldReturn` 2
    setRace 1 0 0
    getEnemyThreat veteran `shouldReturn` 4
    run $ EngageEnemy pid veteran
    (.placement) <$> getEnemy veteran `shouldReturn` StagingArea
    racer <- stage Enemies.aggressiveRacer
    run $ EngageEnemy pid racer
    shadowAttack pid racer Enemies.veteranWainrider =<< beravor
    -- 4 attack, +2, against Beravor's 2 defense: her 4 hit points
    getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` notElem "01012")

  it "lets the players remove damage from The Wainriders' stage in place of damage to Wainrider Champion while Narrow Pass is active" . raceTest $ \pid -> do
    setUp pid
    setRace 0 0 3
    pass <- stageLocation Locations.narrowPass
    run $ PlaceLocation pass ActiveLocation
    champion <- enemyOf Enemies.wainriderChampion
    run $ DealDamage GameSource (toTarget champion) 2
    chooseAbilityOf pid pass
    getDamage (toTarget champion) `shouldReturn` 0
    (.wainridersDamage) <$> getRace `shouldReturn` 1

  it "gives Crowded Track +1 Threat for each character the first player controls, and has each player add an enemy as it becomes active" . raceTest $ \pid -> do
    setUp pid
    track <- stageLocation Locations.crowdedTrack
    getLocationThreat track `shouldReturn` 3
    _ <- setEncounterDeck [Locations.narrowPass, Enemies.veteranWainrider]
    run $ TravelTo track
    chooseOptionMatching "choose the enemy" pid (const True)
    sort <$> enemyCodes `shouldReturn` sort [toCardCode Enemies.wainriderChampion, toCardCode Enemies.veteranWainrider]

  it "attaches Wainrider Chariot to a non-unique Easterling enemy: +1 Attack, a Weapon, its excess damage on The Wainriders' stage" . raceTest $ \pid -> do
    setUp pid
    racer <- stage Enemies.aggressiveRacer
    _ <- setEncounterDeck [Treacheries.wainriderChariot, Locations.narrowPass]
    run RevealEncounterCard
    resolveRevealed
    attachedTo racer `shouldReturn` [toCardCode Treacheries.wainriderChariot]
    champion <- enemyOf Enemies.wainriderChampion
    attachedTo champion `shouldReturn` []
    getEnemyAttack racer `shouldReturn` 5
    setRace 1 0 0
    run $ EngageEnemy pid racer
    guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    shadowAttack pid racer Locations.narrowPass guardian
    -- 5 attack against 2 hit points: 3 excess, and 2 for destroying the Guard, past the
    -- stage's 4 quest points
    (\r -> (r.wainriders, r.wainridersDamage)) <$> getRace `shouldReturn` (1, 1)

  it "has The Champion's Cunning place the Champion's Threat in damage with no one engaged with it, or attack each engaged player" . raceTest $ \pid -> do
    setUp pid
    setRace 1 0 0
    _ <- setEncounterDeck [Treacheries.theChampionsCunning]
    run RevealEncounterCard
    resolveRevealed
    (.wainridersDamage) <$> getRace `shouldReturn` 2
    setRace 0 0 0
    _ <- setEncounterDeck [Treacheries.theChampionsCunning, Locations.narrowPass]
    run RevealEncounterCard
    resolveRevealed
    passActions pid
    b <- beravor
    chooseTarget pid b
    passActions pid
    -- 4 attack against Beravor's 2 defense
    getDamage b `shouldReturn` 2
    (.wainridersDamage) <$> getRace `shouldReturn` 0

  it "gives the attacking enemy +1 Attack with The Champion's Cunning's shadow, and 2 damage on The Wainriders' stage if it destroys a character" . raceTest $ \pid -> do
    setUp pid
    setRace 1 0 0
    racer <- stage Enemies.aggressiveRacer
    run $ EngageEnemy pid racer
    guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    shadowAttack pid racer Treacheries.theChampionsCunning guardian
    -- 2 for the shadow, 2 for Aggressive Racer's own Forced effect: the stage's 4 quest points
    (\r -> (r.wainriders, r.wainridersDamage)) <$> getRace `shouldReturn` (1, 0)

{- | The enemy attacks the player, the shadow card dealt to it first, and the defender
declares against it.
-}
shadowAttack :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
shadowAttack pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid
