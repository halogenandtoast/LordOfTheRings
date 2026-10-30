module LordOfTheRings.LostRealm.TheWeatherHillsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getCommittedCharacters, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getTimeCounters)
import LordOfTheRings.Message.Lifted (endCurrentPhase)
import LordOfTheRings.Modifier (ModifierType (Blank, CannotBeTraveledTo))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveTokens))
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theWeatherHills)
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (Meta (..), getMeta)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), encounterDiscardL, metaL, questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins, PhaseEnds), mkAfter, mkWhen)
import TestImport

hillsTest :: (PlayerId -> GameT ()) -> IO ()
hillsTest = gameTestIn theWeatherHills testDeck

overScenario :: (ScenarioAttrs -> ScenarioAttrs) -> GameT ()
overScenario f = putGame . (scenarioL %~ overAttrs f) =<< getGame

-- | The Orc deck, top first.
setOrcDeck :: [CardDef] -> GameT ()
setOrcDeck defs = do
  cards <- traverse genEncounterCard defs
  overScenario (metaL .~ toJSON (Meta cards))

orcDeckCodes :: GameT [CardCode]
orcDeckCodes = getsGame (map toCardCode . (.orcDeck) . getMeta . toAttrs . (.scenario))

codes :: [EncounterCard] -> [CardCode]
codes = map toCardCode

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [o.placement | o <- toList g.entities.objectives, toCardCode o == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

objectiveOf :: CardDef -> GameT ObjectiveId
objectiveOf def = getsGame (fromJustNote "no such objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

resourcesOn :: CardDef -> GameT Int
resourcesOn def = do
  oid <- objectiveOf def
  getsGame (maybe 0 (countTokens Resource . objectiveTokens . toAttrs) . Map.lookup oid . (.entities.objectives))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

activeAt :: CardDef -> GameT LocationId
activeAt def = do
  run . PutIntoPlayAsActiveLocation =<< genEncounterCard def
  locationOf def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Scattered Among the Hills, flipped to its B side.
stageOne :: GameT Target
stageOne = do
  q <- putQuestStage Quests.scatteredAmongTheHills
  run $ Flip q
  pure q

-- | Destroy an enemy put into the staging area.
defeat :: CardDef -> GameT ()
defeat def = do
  staged def
  eid <- enemyOf def
  run $ DealDamage GameSource (toTarget eid) 5

-- | Resolve the forced effects waiting on the same window in the order offered.
resolveInOrder :: PlayerId -> GameT ()
resolveInOrder pid =
  getQuestion pid >>= \case
    Just (ChooseOneAtATime _) -> answerQuestion pid 0 >> resolveInOrder pid
    _ -> pure ()

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

spec :: Spec
spec = describe "The Weather Hills" do
  it "is a scenario the game can be dealt" . hillsTest $ \_ ->
    isJust (lookupScenario "the-weather-hills") `shouldBe` True

  it "makes the Orc deck, sets Amon Forn aside, and stages Exposed Ridge, Hunting the Orcs and a Weathered Hilltop" . hillsTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    -- the Concealed Orc-camps and the Angmar Orcs enemies went to the Orc deck as the
    -- encounter deck was gathered: 11 cards, 3 of them the camps
    orcDeck <- orcDeckCodes
    length orcDeck `shouldBe` 11
    filter (== toCardCode Locations.concealedOrcCamp) orcDeck `shouldBe` replicate 3 (toCardCode Locations.concealedOrcCamp)
    _ <- setEncounterDeck [Locations.amonForn, Locations.exposedRidge, Objectives.huntingTheOrcs, Locations.weatheredHilltop, Locations.weatheredHilltop]
    chooseOptionMatching "flip the quest" pid (const True)
    placementOf Locations.exposedRidge `shouldReturn` Just ActiveLocation
    placementOf Objectives.huntingTheOrcs `shouldReturn` Just StagingArea
    placementOf Locations.weatheredHilltop `shouldReturn` Just StagingArea
    getsGame (codes . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.amonForn]
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Locations.weatheredHilltop]

  -- \* Scattered Among the Hills

  it "gives the first Weather treachery each round surge, and Weathered Hilltop a resource for each" . hillsTest $ \pid -> do
    _ <- stageOne
    staged Locations.weatheredHilltop
    hilltop <- locationOf Locations.weatheredHilltop
    reveal [Treacheries.bitingWind, Locations.shelteredValley]
    -- the stage's surge and the Hilltop's token answer the same reveal, in either order
    resolveInOrder pid
    placementOf Locations.shelteredValley `shouldReturn` Just StagingArea
    getLocationThreat hilltop `shouldReturn` 2
    reveal [Treacheries.bitingWind, Locations.exposedRidge]
    placementOf Locations.exposedRidge `shouldReturn` Nothing
    getLocationThreat hilltop `shouldReturn` 3

  it "reveals the top card of the Orc deck as the active location is explored" . hillsTest $ \_ -> do
    _ <- stageOne
    setOrcDeck [Enemies.corneredOrc, Locations.concealedOrcCamp]
    ridge <- activeAt Locations.exposedRidge
    run $ PlaceTokens GameSource (toTarget ridge) Progress 3
    placementOf Enemies.corneredOrc `shouldReturn` Just StagingArea
    orcDeckCodes `shouldReturn` [toCardCode Locations.concealedOrcCamp]

  -- \* Hunting the Orcs and Savage Counter-attack

  it "flips Hunting the Orcs into Savage Counter-attack with its tokens once enough enemies are defeated, and advances" . hillsTest $ \pid -> do
    _ <- stageOne
    later <- genEncounterCard Quests.corneredAnimals
    overScenario (questDeckL .~ [later])
    staged Objectives.huntingTheOrcs
    replicateM_ 3 (defeat Enemies.corneredOrc)
    resourcesOn Objectives.huntingTheOrcs `shouldReturn` 3
    defeat Enemies.corneredOrc
    placementOf Objectives.huntingTheOrcs `shouldReturn` Nothing
    resourcesOn Objectives.savageCounterattack `shouldReturn` 4
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.corneredAnimals
    resolveWhenRevealed pid
    outcome `shouldReturn` Nothing

  it "loses a token from Savage Counter-attack as an attack destroys a character, and the game once none are left" . hillsTest $ \_ -> do
    staged Objectives.savageCounterattack
    savage <- objectiveOf Objectives.savageCounterattack
    run $ PlaceTokens GameSource (toTarget savage) Resource 2
    eid <- (staged Enemies.corneredOrc >> enemyOf Enemies.corneredOrc)
    eowyn <- hero "01007"
    run $ Destroy (EnemyAttackSource eid) eowyn
    resourcesOn Objectives.savageCounterattack `shouldReturn` 1
    outcome `shouldReturn` Nothing
    run $ RemoveTokens GameSource (toTarget savage) Resource 1
    outcome `shouldReturn` Just Lost

  -- \* Cornered Animals

  it "ends the phase, shuffles the discard pile and Orc deck in, and reveals Orc Ambush and Amon Forn" . hillsTest $ \pid -> do
    forn <- genEncounterCard Locations.amonForn
    storm <- genEncounterCard Treacheries.iceStorm
    overScenario ((setAsideL .~ [forn]) . (encounterDiscardL .~ [storm]))
    setOrcDeck [Enemies.corneredOrc]
    _ <- setEncounterDeck []
    q <- putQuestStage Quests.corneredAnimals
    run $ Flip q
    resolveWhenRevealed pid
    placementOf Locations.amonForn `shouldReturn` Just StagingArea
    getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` []
    getEncounterDiscard `shouldReturn` []
    sort . codes <$> getEncounterDeck `shouldReturn` sort [toCardCode Treacheries.iceStorm, toCardCode Enemies.corneredOrc]
    orcDeckCodes `shouldReturn` []

  it "drops what is left of the phase as it ends" . hillsTest $ \pid -> do
    phase <- getsGame gamePhase
    threat <- (.threat) <$> getPlayer pid
    pushAll [RaiseThreat pid 5, EndPhase phase]
    runQueueT endCurrentPhase
    _ <- mainLoop
    (.threat) <$> getPlayer pid `shouldReturn` threat

  it "gives each damaged character -1 willpower, and costs a token or an extra reveal as the quest phase begins" . hillsTest $ \pid -> do
    q <- putQuestStage Quests.corneredAnimals
    run $ Flip q
    resolveWhenRevealed pid
    eowyn <- hero "01007"
    willpower <- getWillpower eowyn
    run $ DealDamage GameSource eowyn 1
    getWillpower eowyn `shouldReturn` willpower - 1
    staged Objectives.savageCounterattack
    savage <- objectiveOf Objectives.savageCounterattack
    run $ PlaceTokens GameSource (toTarget savage) Resource 2
    run $ CheckWindows [mkAfterPhase QuestPhase]
    chooseLabel "$label.corneredAnimals.discard" pid
    resourcesOn Objectives.savageCounterattack `shouldReturn` 1

  it "cannot be won while Amon Forn is in play" . hillsTest $ \pid -> do
    q <- putQuestStage Quests.corneredAnimals
    run $ Flip q
    resolveWhenRevealed pid
    staged Locations.amonForn
    run $ PlaceTokens GameSource q Progress 20
    outcome `shouldReturn` Nothing
    run . AddToVictoryDisplay . toTarget =<< locationOf Locations.amonForn
    outcome `shouldReturn` Just Won

  -- \* Locations

  it "has an Orc added to the staging area attack the first player while Amon Forn is active" . hillsTest $ \pid -> do
    _ <- activeAt Locations.amonForn
    staged Enemies.corneredOrc
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2

  it "has each player deal 1 damage at the end of the refresh phase while Exposed Ridge is active" . hillsTest $ \pid -> do
    _ <- activeAt Locations.exposedRidge
    run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1

  it "costs a token from the Mission objective to travel to Sheltered Valley" . hillsTest $ \_ -> do
    staged Objectives.huntingTheOrcs
    staged Locations.shelteredValley
    valley <- locationOf Locations.shelteredValley
    (CannotBeTraveledTo `elem`) <$> getModifiers valley `shouldReturn` True
    hunting <- objectiveOf Objectives.huntingTheOrcs
    run $ PlaceTokens GameSource (toTarget hunting) Resource 1
    (CannotBeTraveledTo `elem`) <$> getModifiers valley `shouldReturn` False
    run $ TravelTo valley
    resourcesOn Objectives.huntingTheOrcs `shouldReturn` 0

  it "adds a token to the Mission objective as the active Concealed Orc-camp is explored" . hillsTest $ \pid -> do
    staged Objectives.huntingTheOrcs
    camp <- activeAt Locations.concealedOrcCamp
    run $ PlaceTokens GameSource (toTarget camp) Progress 2
    chooseAbilityOf pid camp
    resourcesOn Objectives.huntingTheOrcs `shouldReturn` 1

  it "engages the first player with an Orc as the players travel to Concealed Orc-camp" . hillsTest $ \pid -> do
    [orc] <- setEncounterDeck [Enemies.corneredOrc]
    staged Locations.concealedOrcCamp
    camp <- locationOf Locations.concealedOrcCamp
    run $ TravelTo camp
    chooseTarget pid (CardIdTarget orc.id)
    placementOf Enemies.corneredOrc `shouldReturn` Just (EngagedWith pid)

  -- \* Treacheries

  it "has each player assign damage for the quest cards with Ice Storm, then exhausts each damaged character" . hillsTest $ \pid -> do
    _ <- putQuestStage Quests.corneredAnimals
    reveal [Treacheries.iceStorm]
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False

  it "assigns a damage for each committed character among them with Biting Wind" . hillsTest $ \pid -> do
    beravor <- hero "01012"
    eowyn <- hero "01007"
    run $ CommitToQuest beravor
    run $ CommitToQuest eowyn
    reveal [Treacheries.bitingWind]
    chooseTarget pid beravor
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 2

  it "removes each damaged character from the quest with Freezing Blast" . hillsTest $ \_ -> do
    beravor <- hero "01012"
    eowyn <- hero "01007"
    run $ CommitToQuest beravor
    run $ CommitToQuest eowyn
    run $ DealDamage GameSource beravor 1
    reveal [Treacheries.freezingBlast]
    getCommittedCharacters `shouldReturn` [eowyn]

  it "blanks each damaged character once Cold from Angmar is attached to the current quest" . hillsTest $ \pid -> do
    _ <- putQuestStage Quests.corneredAnimals
    reveal [Treacheries.coldFromAngmar]
    beravor <- hero "01012"
    chooseTarget pid beravor
    getsGame (map (.placement) . toList . (.entities.treacheries)) >>= (`shouldSatisfy` all isAttached)
    (Blank `elem`) <$> getModifiers beravor `shouldReturn` True
    (Blank `elem`) <$> (getModifiers =<< hero "01007") `shouldReturn` False

  -- \* Side quests

  it "deals 4 damage to a hero of each player as Find Shelter's last time counter goes, and resets" . hillsTest $ \pid -> do
    _ <- putQuestStage Quests.corneredAnimals
    reveal [Quests.findShelter]
    shelter <- getsGame (toTarget . fromJustNote "no Find Shelter" . find ((== toCardCode Quests.findShelter) . toCardCode) . toList . (.entities.quests))
    getTimeCounters shelter `shouldReturn` 4
    run $ RemoveTokens GameSource shelter TimeCounter 4
    -- Éowyn has 3 hit points left of 3: the 4 damage destroys her
    chooseTarget pid =<< hero "01007"
    getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` notElem "01007")
    getTimeCounters shelter `shouldReturn` 4

  it "bars healing while Make Camp is in play, and heals a hero once it is defeated" . hillsTest $ \_ -> do
    main <- putQuestStage Quests.corneredAnimals
    reveal [Quests.makeCamp]
    camp <- getsGame (toTarget . fromJustNote "no Make Camp" . find ((== toCardCode Quests.makeCamp) . toCardCode) . toList . (.entities.quests))
    beravor <- hero "01012"
    run $ DealDamage GameSource beravor 3
    run $ HealDamage GameSource beravor 2
    getDamage beravor `shouldReturn` 3
    run $ SetCurrentQuest camp
    run $ PlaceTokens GameSource camp Progress 6
    -- Beravor is the one damaged hero: no choice is asked
    getDamage beravor `shouldReturn` 0
    getsGame (map toTarget . toList . (.entities.quests)) `shouldReturn` [main]
 where
  isAttached = \case
    AttachedTo (QuestTarget _) -> True
    _ -> False
  mkAfterPhase = mkAfter . PhaseBegins
