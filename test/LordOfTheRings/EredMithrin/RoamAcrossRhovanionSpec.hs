module LordOfTheRings.EredMithrin.RoamAcrossRhovanionSpec (spec) where

import LordOfTheRings.Card (CardDef (cdEncounterSet), CardType (..), cdCardType, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense, getEnemyThreat, getStagingThreat)
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (AllyMatcher (..), HeroMatcher (..), PlayerMatcher (..), getEnemy, select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs)
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, roamAcrossRhovanion)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

rarTest :: (PlayerId -> GameT ()) -> IO ()
rarTest = gameTestIn roamAcrossRhovanion testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

setSetAside :: [CardDef] -> GameT ()
setSetAside defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

-- | A stage put into play on its B side, its When Revealed not resolved.
stageB :: CardDef -> GameT Target
stageB def = do
  q <- putQuestStage def
  g <- getGame
  putGame =<< preloadModifiers (g & entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True}))
  pure q

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

allyOf :: CardDef -> GameT (Maybe AllyAttrs)
allyOf def = getsGame (fmap toAttrs . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

objectiveOf :: CardDef -> GameT (Maybe ObjectiveAttrs)
objectiveOf def = getsGame (fmap toAttrs . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

-- | Urdug, objective-ally side, under the player's control.
urdugUnder :: PlayerId -> GameT AllyId
urdugUnder pid = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.urdug
  fromJustNote "no Urdug" . fmap (.id) <$> allyOf Objectives.urdug

controls :: PlayerId -> CardDef -> GameT Bool
controls pid def = notNull <$> select (AllyWithCardCode (toCardCode def) <> AllyControlledBy (PlayerWithId pid))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stageTwos :: [CardCode]
stageTwos = map toCardCode [Quests.rescueTiny, Quests.retrieveUrdugsHorn, Quests.findDurinsKey]

spec :: Spec
spec = describe "Roam Across Rhovanion" do
  it "is a scenario the game can be dealt" . rarTest $ \_ ->
    isJust (lookupScenario "roam-across-rhovanion") `shouldBe` True

  -- \* Setup

  it "builds the encounter deck from its four sets, gives Urdug to the first player, sets the other objectives aside and stages a location" . rarTest $ \pid -> do
    flipFirstQuest pid
    s <- scenarioAttrs
    let everything = s.encounterDeck <> s.encounterDiscard
        setOf = cdEncounterSet . toCardDef
    all ((`elem` map Just [RoamAcrossRhovanion, FellBeasts, LostInWilderland, HillsOfWilderland]) . setOf) everything `shouldBe` True
    any ((`elem` [ObjectiveType, ObjectiveAllyType]) . cdCardType . toCardDef) everything `shouldBe` False
    sort (map toCardCode (scenarioSetAside s)) `shouldBe` sort (map toCardCode [Objectives.durinsKey, Objectives.urdugsHorn, Objectives.tiny])
    controls pid Objectives.urdug `shouldReturn` True
    staged <- getsGame (toList . (.entities.locations))
    length staged `shouldBe` 1
    activeQuestCode `shouldReturn` toCardCode Quests.theGoblinsTask
    (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True
    length s.questDeck `shouldBe` 4
    -- the Urdug enemy side and Tiny's are never gathered
    enemyCodes `shouldReturn` []

  -- \* The Goblin's Task

  it "keeps progress off staged locations on stage 1B and advances to a random stage 2 as the active location is explored with Urdug" . rarTest $ \pid -> do
    flipFirstQuest pid
    staged <- getsGame (map toId . toList . (.entities.locations))
    for_ staged \l -> getModifiers (LocationTarget l) >>= (`shouldSatisfy` elem (ProgressLimit 0))
    _ <- setEncounterDeck [Treacheries.harshWeather]
    for_ (take 1 staged) \l -> do
      run $ PlaceLocation l ActiveLocation
      run $ CheckWindows [mkWhen $ ActiveLocationExplored l]
    activeQuestCode >>= (`shouldSatisfy` (`elem` stageTwos))
    length . (.questDeck) <$> scenarioAttrs `shouldReturn` 3

  it "stays on stage 1B as the active location is explored without Urdug" . rarTest $ \pid -> do
    flipFirstQuest pid
    urdug <- fromJustNote "no Urdug" <$> allyOf Objectives.urdug
    l <- stageLocation Locations.slopesOfEredMithrin
    run . GuardCharacter (toTarget urdug.id) . (.cardId) . toAttrs =<< getLocation l
    lid <- stageLocation Locations.deepRavine
    run $ PlaceLocation lid ActiveLocation
    run $ CheckWindows [mkWhen $ ActiveLocationExplored lid]
    activeQuestCode `shouldReturn` toCardCode Quests.theGoblinsTask

  it "draws stage 3 once no stage 2 is left in the quest deck" . rarTest $ \_ -> do
    setQuestDeck [Quests.urdugsGambit]
    _ <- putQuestStage Quests.findDurinsKey
    run AdvanceQuest
    getsGame (map toCardCode . toList . (.entities.quests)) >>= (`shouldSatisfy` elem (toCardCode Quests.urdugsGambit))

  -- \* Rescue Tiny

  it "guards Tiny with a Grey Moorland it adds to the staging area, which cannot be traveled to under 5 progress, and is defeated once Tiny is free" . rarTest $ \pid -> do
    _ <- urdugUnder pid
    setSetAside [Objectives.tiny]
    setQuestDeck [Quests.urdugsGambit]
    _ <- setEncounterDeck [Locations.greyMoorland, Treacheries.harshWeather]
    q <- putQuestStage Quests.rescueTiny
    run $ Flip q
    resolveWhenRevealed pid
    moor <- locationOf Locations.greyMoorland
    (.placement) <$> getLocation moor `shouldReturn` StagingArea
    controls pid Objectives.tiny `shouldReturn` False
    isJust <$> allyOf Objectives.tiny `shouldReturn` True
    getModifiers (LocationTarget moor) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ PlaceTokens GameSource q Progress 6
    getModifiers (LocationTarget moor) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    -- not defeated while Tiny is guarded
    activeQuestCode `shouldReturn` toCardCode Quests.rescueTiny
    run $ Discard (LocationTarget moor)
    controls pid Objectives.tiny `shouldReturn` True
    activeQuestCode `shouldReturn` toCardCode Quests.urdugsGambit

  it "keeps the When Revealed effects of treacheries from being canceled on stage 2B of Rescue Tiny" . rarTest $ \_ -> do
    _ <- stageB Quests.rescueTiny
    run . RevealCard =<< genEncounterCard Treacheries.sneakingOff
    ts <- getsGame (map toId . toList . (.entities.treacheries))
    for_ ts \t -> getModifiers (TreacheryTarget t) >>= (`shouldSatisfy` elem WhenRevealedCannotBeCanceled)

  -- \* Retrieve Urdug's Horn

  it "guards Urdug's Horn with a Grey Mountain Giant, +2 threat and defense and no damage under 5 progress, then attaches it to Urdug" . rarTest $ \pid -> do
    urdug <- urdugUnder pid
    setSetAside [Objectives.urdugsHorn]
    setQuestDeck [Quests.urdugsGambit]
    _ <- setEncounterDeck [Enemies.greyMountainGiant, Treacheries.harshWeather]
    q <- putQuestStage Quests.retrieveUrdugsHorn
    run $ Flip q
    resolveWhenRevealed pid
    giant <- enemyOf Enemies.greyMountainGiant
    getEnemyThreat giant `shouldReturn` 4
    getEnemyDefense giant `shouldReturn` 4
    getModifiers (EnemyTarget giant) >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ PlaceTokens GameSource q Progress 5
    getModifiers (EnemyTarget giant) >>= (`shouldSatisfy` notElem CannotTakeDamage)
    activeQuestCode `shouldReturn` toCardCode Quests.retrieveUrdugsHorn
    run $ Destroy GameSource (EnemyTarget giant)
    fmap (.placement) <$> objectiveOf Objectives.urdugsHorn `shouldReturn` Just (AttachedTo (AllyTarget urdug))
    activeQuestCode `shouldReturn` toCardCode Quests.urdugsGambit

  it "adds the threat of the card Urdug's Horn discards to Urdug's attack as he attacks" . rarTest $ \pid -> do
    urdug <- urdugUnder pid
    card <- genEncounterCard Objectives.urdugsHorn
    run $ PutObjectiveIntoPlayAttached card (AllyTarget urdug)
    _ <- setEncounterDeck [Locations.greyMoorland]
    eid <- stage Enemies.huntingEagle
    run $ CheckWindows [mkAfter $ DeclaredAsAttacker (AllyTarget urdug) eid]
    getModifiers (AllyTarget urdug) >>= (`shouldSatisfy` elem (AttackModifier 3))

  -- \* Find Durin's Key

  it "guards Durin's Key with a Deep Ravine, immune to player cards, then attaches it to a hero as the ravine is explored" . rarTest $ \pid -> do
    _ <- urdugUnder pid
    setSetAside [Objectives.durinsKey]
    setQuestDeck [Quests.urdugsGambit]
    _ <- setEncounterDeck [Locations.deepRavine, Treacheries.harshWeather]
    q <- putQuestStage Quests.findDurinsKey
    run $ Flip q
    resolveWhenRevealed pid
    ravine <- locationOf Locations.deepRavine
    mods <- getModifiers (LocationTarget ravine)
    mods `shouldSatisfy` elem ImmuneToPlayerCardEffects
    mods `shouldSatisfy` elem CannotBeTraveledTo
    run $ PlaceTokens GameSource q Progress 5
    run $ Discard (LocationTarget ravine)
    beravor <- hero "01012"
    chooseTarget pid beravor
    fmap (.placement) <$> objectiveOf Objectives.durinsKey `shouldReturn` Just (AttachedTo beravor)
    activeQuestCode `shouldReturn` toCardCode Quests.urdugsGambit

  it "has the player with Durin's Key reveal 1 of the top 3 encounter cards as the quest phase begins, discarding the others" . rarTest $ \pid -> do
    beravor <- hero "01012"
    card <- genEncounterCard Objectives.durinsKey
    run $ PutObjectiveIntoPlayAttached card beravor
    [a, b, c] <- setEncounterDeck [Treacheries.harshWeather, Locations.greyMoorland, Locations.deepRavine]
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    chooseTarget pid c
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.deepRavine]
    map (.id) . (.encounterDiscard) <$> scenarioAttrs >>= (`shouldSatisfy` \d -> a.id `elem` d && b.id `elem` d)

  it "gives each location -1 threat this phase as Durin's Key exhausts" . rarTest $ \pid -> do
    beravor <- hero "01012"
    card <- genEncounterCard Objectives.durinsKey
    run $ PutObjectiveIntoPlayAttached card beravor
    _ <- stageLocation Locations.greyMoorland
    getStagingThreat `shouldReturn` 3
    run $ BeginPhase QuestPhase
    key <- fromJustNote "no key" <$> objectiveOf Objectives.durinsKey
    chooseAbilityOf pid (ObjectiveTarget key.id)
    getStagingThreat `shouldReturn` 2

  -- \* Urdug's Gambit

  it "flips Urdug and Tiny to their enemy sides in the staging area, Urdug keeping his horn" . rarTest $ \pid -> do
    urdug <- urdugUnder pid
    run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.tiny
    card <- genEncounterCard Objectives.urdugsHorn
    run $ PutObjectiveIntoPlayAttached card (AllyTarget urdug)
    run $ PlaceTokens GameSource (AllyTarget urdug) Damage 2
    q <- putQuestStage Quests.urdugsGambit
    run $ Flip q
    resolveWhenRevealed pid
    sort <$> enemyCodes `shouldReturn` sort (map toCardCode [Enemies.urdugEnemy, Enemies.tinyEnemy])
    u <- enemyOf Enemies.urdugEnemy
    (.placement) <$> getEnemy u `shouldReturn` StagingArea
    getDamage (EnemyTarget u) `shouldReturn` 0
    fmap (.placement) <$> objectiveOf Objectives.urdugsHorn `shouldReturn` Just (AttachedTo (EnemyTarget u))
    getsGame (length . (.entities.allies)) `shouldReturn` 0

  it "places progress instead of raising threat as the players quest unsuccessfully on stage 3B, losing at 5" . rarTest $ \pid -> do
    _ <- stageB Quests.urdugsGambit
    _ <- stageLocation Locations.greyMoorland
    threat <- threatOf pid
    inPhase QuestPhase
    run QuestResolution
    threatOf pid `shouldReturn` threat
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 3
    getsGame (.outcome) `shouldReturn` Nothing
    run QuestResolution
    getsGame (.outcome) `shouldReturn` Just Lost

  it "places no progress on stage 3B from questing successfully" . rarTest $ \_ -> do
    _ <- stageB Quests.urdugsGambit
    run $ PlaceProgressOnQuest 3
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0

  it "keeps damage off Urdug while Tiny is in play, and wins once Urdug has no hit points left" . rarTest $ \_ -> do
    _ <- stageB Quests.urdugsGambit
    u <- stage Enemies.urdugEnemy
    t <- stage Enemies.tinyEnemy
    getModifiers (EnemyTarget u) >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ AddToVictoryDisplay (EnemyTarget t)
    run $ DealDamage GameSource (EnemyTarget u) 5
    isJust <$> getsGame (lookup u . (.entities.enemies)) `shouldReturn` True
    getsGame (.outcome) `shouldReturn` Just Won

  it "has Tiny engage the player Urdug engages" . rarTest $ \pid -> do
    u <- stage Enemies.urdugEnemy
    t <- stage Enemies.tinyEnemy
    run $ EngageEnemy pid u
    (.placement) <$> getEnemy t `shouldReturn` EngagedWith pid

  it "has an engaged Urdug attack as the quest phase begins, returning him and Tiny to the staging area if it deals damage" . rarTest $ \pid -> do
    u <- stage Enemies.urdugEnemy
    t <- stage Enemies.tinyEnemy
    run $ EngageEnemy pid u
    _ <- setEncounterDeck [Locations.deepRavine]
    inPhase QuestPhase
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid =<< hero "01012"
    passActions pid
    (.placement) <$> getEnemy u `shouldReturn` StagingArea
    (.placement) <$> getEnemy t `shouldReturn` StagingArea

  -- \* Urdug and Tiny

  it "loses the game as Urdug or Tiny, objective-ally side, is destroyed" . rarTest $ \pid -> do
    urdug <- urdugUnder pid
    run $ DealDamage GameSource (AllyTarget urdug) 5
    getsGame (.outcome) `shouldReturn` Just Lost

  it "raises Tiny's controller's threat by 2 as he exhausts, and follows the first player" . rarTest $ \pid -> do
    run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.tiny
    tiny <- fromJustNote "no Tiny" <$> allyOf Objectives.tiny
    threat <- threatOf pid
    run $ Exhaust (AllyTarget tiny.id)
    threatOf pid `shouldReturn` threat + 2
    getModifiers (AllyTarget tiny.id) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  -- \* Encounter cards

  it "attaches Urdug to a staged location for Sneaking Off, removed from the game" . rarTest $ \pid -> do
    urdug <- urdugUnder pid
    l <- stageLocation Locations.slopesOfEredMithrin
    reveal [Treacheries.sneakingOff]
    controls pid Objectives.urdug `shouldReturn` False
    lc <- (.cardId) . toAttrs <$> getLocation l
    fmap (.placement) <$> allyOf Objectives.urdug `shouldReturn` Just (AttachedTo (CardIdTarget lc))
    map toCardCode . (.encounterDiscard) <$> scenarioAttrs `shouldReturn` []
    run $ Discard (LocationTarget l)
    controls pid Objectives.urdug `shouldReturn` True
    void $ pure urdug

  it "adds 5 to the staging area's threat for Sneaking Off when Urdug cannot be attached" . rarTest $ \_ -> do
    reveal [Treacheries.sneakingOff]
    getStagingThreat `shouldReturn` 5

  it "discards the top 5 cards of the player's deck as Grey Mountain Giant engages him, assigning damage for each ally" . rarTest $ \pid -> do
    deck <- take 5 . (.deck) <$> getPlayer pid
    let allies = length (filter ((== AllyType) . cdCardType . toCardDef) deck)
    giant <- stage Enemies.greyMountainGiant
    run $ EngageEnemy pid giant
    discard <- length . (.discard) <$> getPlayer pid
    discard `shouldBe` 5
    getQuestion pid >>= (`shouldSatisfy` \case
      Just (QuestionLabel l _ _ _) -> l == "$label.dealDamageDivided n=i:" <> tshow allies
      _ -> False)

  it "has Hunting Eagle engage the first player as the encounter phase begins, never optionally" . rarTest $ \pid -> do
    eagle <- stage Enemies.huntingEagle
    getModifiers (EnemyTarget eagle) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))
    inPhase EncounterPhase
    run $ CheckWindows [mkAfter $ PhaseBegins EncounterPhase]
    (.placement) <$> getEnemy eagle `shouldReturn` EngagedWith pid

  it "has each player discard a random card for Wilderland Bear when the first player chooses so" . rarTest $ \pid -> do
    replicateM_ 2 $ putCardIntoHand pid Events.swiftStrike
    reveal [Enemies.wilderlandBear]
    chooseLabel "$label.wilderlandBear.discard" pid
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "resolves the When Revealed of the first treachery discarded as Grey Moorland becomes active" . rarTest $ \pid -> do
    moor <- stageLocation Locations.greyMoorland
    _ <- setEncounterDeck [Treacheries.sneakingOff]
    run $ PlaceLocation moor ActiveLocation
    resolveRevealed
    getStagingThreat `shouldReturn` 5
    void $ pure pid

  it "lets only 1 progress a round onto staged locations while Deep Ravine is staged" . rarTest $ \_ -> do
    ravine <- stageLocation Locations.deepRavine
    moor <- stageLocation Locations.greyMoorland
    run $ PlaceTokens GameSource (LocationTarget moor) Progress 3
    (.progress) <$> getLocation moor `shouldReturn` 1
    run $ PlaceTokens GameSource (LocationTarget ravine) Progress 2
    (.progress) <$> getLocation ravine `shouldReturn` 0

  it "has the first player exhaust Urdug or raise each player's threat by 2 to travel to Deep Ravine" . rarTest $ \pid -> do
    urdug <- urdugUnder pid
    ravine <- stageLocation Locations.deepRavine
    run $ TravelTo ravine
    chooseLabel "$label.deepRavine.exhaustUrdug" pid
    fmap (.allyExhausted) <$> allyOf Objectives.urdug `shouldReturn` Just True
    void $ pure urdug

  it "exhausts Urdug with Slopes of Ered Mithrin's shadow" . rarTest $ \pid -> do
    _ <- urdugUnder pid
    eid <- stage Enemies.huntingEagle
    run $ EngageEnemy pid eid
    _ <- setEncounterDeck [Locations.slopesOfEredMithrin]
    run $ DealShadowCard eid
    inPhase CombatPhase
    run $ EnemyAttacks pid
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    fmap (.allyExhausted) <$> allyOf Objectives.urdug `shouldReturn` Just True

  it "has each player exhaust each ready character or raise his threat for Harsh Weather" . rarTest $ \pid -> do
    threat <- threatOf pid
    reveal [Treacheries.harshWeather]
    chooseLabel "$label.harshWeather.raiseThreat" pid
    chooseLabel "$label.harshWeather.exhaust" pid
    chooseLabel "$label.harshWeather.exhaust" pid
    threatOf pid `shouldReturn` threat + 1
    length <$> select (HeroControlledBy (PlayerWithId pid) <> ExhaustedHero) `shouldReturn` 2

  it "has each player engage a staged enemy for Wild Creatures, discarding to a Creature enemy when one engages none" . rarTest $ \pid -> do
    _ <- setEncounterDeck [Treacheries.wildCreatures, Treacheries.harshWeather, Enemies.huntingEagle]
    run RevealEncounterCard
    resolveRevealed
    eagle <- enemyOf Enemies.huntingEagle
    (.placement) <$> getEnemy eagle `shouldReturn` StagingArea
    void $ pure pid
