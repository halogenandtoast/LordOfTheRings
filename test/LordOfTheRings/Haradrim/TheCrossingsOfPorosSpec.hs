module LordOfTheRings.Haradrim.TheCrossingsOfPorosSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdEncounterSet), HasCardDef, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getEngagementCost, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase, PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario (lookupScenario, theCrossingsOfPoros)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), encounterDiscardL, questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds, PlayedCard), mkAfter, mkWhen)
import TestImport

porosTest :: (PlayerId -> GameT ()) -> IO ()
porosTest = gameTestIn theCrossingsOfPoros testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

setAsideCodes :: GameT [CardCode]
setAsideCodes = map toCardCode . scenarioSetAside <$> scenarioAttrs

setOf :: HasCardDef a => a -> Maybe EncounterSet
setOf = cdEncounterSet . toCardDef

setSetAside :: [CardDef] -> GameT [EncounterCard]
setSetAside defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)
  pure cards

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

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
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

-- | A stage put into play on its B side, its When Revealed not resolved.
stageB :: CardDef -> GameT Target
stageB def = do
  q <- putQuestStage def
  g <- getGame
  putGame =<< preloadModifiers (g & entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True}))
  pure q

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

allyOf :: CardDef -> GameT (Maybe AllyAttrs)
allyOf def = getsGame (fmap toAttrs . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

spec :: Spec
spec = describe "The Crossings of Poros" do
  it "is a scenario the game can be dealt" . porosTest $ \_ ->
    isJust (lookupScenario "the-crossings-of-poros") `shouldBe` True

  -- \* Setup

  it "builds the encounter deck from The Crossings of Poros alone, sets the other sets and Poros Garrison aside, and stages The Black Serpent" . porosTest $ \pid -> do
    flipFirstQuest pid
    deck <- getEncounterDeck
    -- 17 cards, less The Black Serpent and Poros Garrison
    length deck `shouldBe` 15
    all ((== Just TheCrossingsOfPoros) . setOf) deck `shouldBe` True
    aside <- setAsideCodes
    aside `shouldSatisfy` elem (toCardCode Objectives.porosGarrison)
    for_ [MountainsOfShadow, DesertSands, DesertCreatures, HaradSoldiers, MordorOrcs] \s ->
      any ((== Just s) . setOf) aside `shouldBe` True
    enemyCodes `shouldReturn` [toCardCode Enemies.theBlackSerpentCrossingsOfPoros]
    serpent <- enemyOf Enemies.theBlackSerpentCrossingsOfPoros
    (.placement) <$> getEnemy serpent `shouldReturn` StagingArea
    activeQuestCode `shouldReturn` toCardCode Quests.flightToGondor
    (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

  -- \* Flight to Gondor

  it "has allies enter play exhausted on stage 1B" . porosTest $ \pid -> do
    flipFirstQuest pid
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    fmap allyExhausted <$> allyOf Allies.guardOfTheCitadel `shouldReturn` Just True

  it "has an ally enter play on stage 1B exhausted already, before its entering windows open" . porosTest $ \pid -> do
    flipFirstQuest pid
    card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
    runOnly $ PutAllyIntoPlayUnder pid card
    fmap allyExhausted <$> allyOf Allies.guardOfTheCitadel `shouldReturn` Just True

  it "advances to a random stage 2 at the end of the planning phase, which shuffles in its set and removes the other" . porosTest $ \pid -> do
    flipFirstQuest pid
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    resolveWhenRevealed pid
    code <- activeQuestCode
    code `shouldSatisfy` (`elem` [toCardCode Quests.theDesertPath, toCardCode Quests.theEphelDuath])
    let (shuffled, removed, other)
          | code == toCardCode Quests.theDesertPath = (DesertSands, MountainsOfShadow, Quests.theEphelDuath)
          | otherwise = (MountainsOfShadow, DesertSands, Quests.theDesertPath)
    aside <- setAsideCodes
    any ((== Just shuffled) . setOf) aside `shouldBe` False
    any ((== Just removed) . setOf) aside `shouldBe` False
    any ((== Just shuffled) . setOf) <$> getEncounterDeck `shouldReturn` True
    any ((== Just removed) . setOf) <$> getEncounterDeck `shouldReturn` False
    map toCardCode . scenarioQuestDeck <$> scenarioAttrs >>= (`shouldSatisfy` notElem (toCardCode other))

  -- \* The Desert Path and The Ephel Dúath

  it "has a player who plays an ally on The Desert Path exhaust it or deal it 1 damage" . porosTest $ \pid -> do
    _ <- stageB Quests.theDesertPath
    _ <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    chooseLabel "$label.theDesertPath.damage" pid
    al <- fromJustNote "no ally" <$> allyOf Allies.guardOfTheCitadel
    getDamage (toTarget $ allyId al) `shouldReturn` 1

  it "has The Desert Path answer an ally played from anywhere, not from hand alone" . porosTest $ \pid -> do
    _ <- stageB Quests.theDesertPath
    card <- genPlayerCard Allies.guardOfTheCitadel (Just pid)
    run $ PutAllyIntoPlayUnder pid card
    -- put into play, it was not played
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    run $ CheckWindows [mkAfter $ PlayedCard pid card]
    chooseLabel "$label.theDesertPath.damage" pid
    al <- fromJustNote "no ally" <$> allyOf Allies.guardOfTheCitadel
    getDamage (toTarget $ allyId al) `shouldReturn` 1

  it "has a player who plays an ally on The Ephel Dúath exhaust it or raise his threat by 1" . porosTest $ \pid -> do
    _ <- stageB Quests.theEphelDuath
    threat0 <- threatOf pid
    _ <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    chooseLabel "$label.theEphelDuath.exhaust" pid
    fmap allyExhausted <$> allyOf Allies.guardOfTheCitadel `shouldReturn` Just True
    threatOf pid `shouldReturn` threat0

  it "advances from a stage 2 to a random stage 3, which sets the others aside and shuffles in its set and the discard pile" . porosTest $ \pid -> do
    setQuestDeck [Quests.deadlyCreatures, Quests.southronPursuit, Quests.orcPatrols, Quests.battleAtTheCrossings]
    _ <- setSetAside [Enemies.giantScorpion, Enemies.southronSoldier, Enemies.urukOfMordor]
    q <- stageB Quests.theDesertPath
    discard <- genEncounterCard Locations.harondorWaste
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [discard])
    _ <- setEncounterDeck []
    run $ PlaceTokens GameSource q Progress 10
    resolveWhenRevealed pid
    code <- activeQuestCode
    let threes = [Quests.deadlyCreatures, Quests.southronPursuit, Quests.orcPatrols]
        others = [toCardCode d | d <- threes, toCardCode d /= code]
    code `shouldSatisfy` (`elem` map toCardCode threes)
    map toCardCode . scenarioQuestDeck <$> scenarioAttrs `shouldReturn` [toCardCode Quests.battleAtTheCrossings]
    aside <- setAsideCodes
    all (`elem` aside) others `shouldBe` True
    -- the stage's set came in with the discard pile: one card of the four is revealed
    s <- scenarioAttrs
    length (scenarioSetAside s) `shouldBe` 4
    scenarioEncounterDiscard s `shouldSatisfy` notElem discard

  -- \* The stage 3s

  it "lets no enemy in the staging area take damage, and deals 1 damage to a hero of a player an enemy engages, on Deadly Creatures" . porosTest $ \pid -> do
    _ <- stageB Quests.deadlyCreatures
    eid <- stage Enemies.hillTroll
    run $ DealDamage GameSource (toTarget eid) 2
    getDamage (toTarget eid) `shouldReturn` 0
    run $ EngageEnemy pid eid
    beravor <- hero "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 1

  it "gives enemies in the staging area -10 engagement cost and removes 1 progress as an enemy engages, on Southron Pursuit" . porosTest $ \pid -> do
    q <- stageB Quests.southronPursuit
    run $ PlaceTokens GameSource q Progress 3
    eid <- stage Enemies.hillTroll
    getEngagementCost eid `shouldReturn` 20
    run $ EngageEnemy pid eid
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 2

  it "gets +2 quest points for each enemy in the staging area and raises the threat of a player an enemy engages, on Orc Patrols" . porosTest $ \pid -> do
    _ <- stageB Quests.orcPatrols
    eid <- stage Enemies.hillTroll
    _ <- stage Enemies.urukOfMordor
    (getQuestPoints . toAttrs =<< getActiveQuest) `shouldReturn` 19
    threat0 <- threatOf pid
    run $ EngageEnemy pid eid
    threatOf pid `shouldReturn` (threat0 + 1)

  -- \* Battle at the Crossings

  it "gives Poros Garrison to the first player and stages the enemies of the sets aside stage 3s shuffled in" . porosTest $ \pid -> do
    _ <- setSetAside [Objectives.porosGarrison, Quests.southronPursuit, Quests.orcPatrols]
    _ <- setEncounterDeck [Enemies.southronSoldier, Enemies.urukOfMordor, Enemies.giantScorpion, Locations.harondorWaste]
    archer <- genEncounterCard Enemies.southronArcher
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ [archer])
    q <- putQuestStage Quests.battleAtTheCrossings
    run $ Flip q
    resolveWhenRevealed pid
    fmap allyPlacement <$> allyOf Objectives.porosGarrison `shouldReturn` Just (InPlayArea pid)
    sort <$> enemyCodes `shouldReturn` sort (map toCardCode [Enemies.southronSoldier, Enemies.urukOfMordor, Enemies.southronArcher])
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Enemies.giantScorpion))
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem OptionallyEngagesAdditionalEnemy)

  it "is defeated only at the end of the combat phase, and the players win" . porosTest $ \_ -> do
    q <- stageB Quests.battleAtTheCrossings
    run $ PlaceTokens GameSource q Progress 20
    activeQuestCode `shouldReturn` toCardCode Quests.battleAtTheCrossings
    getsGame (.outcome) `shouldReturn` Nothing
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    getsGame (.outcome) `shouldReturn` Just Won

  it "loses progress equal to the printed threat of an enemy whose attack destroys a character" . porosTest $ \_ -> do
    q <- stageB Quests.battleAtTheCrossings
    run $ PlaceTokens GameSource q Progress 5
    eid <- stage Enemies.southronSoldier
    beravor <- hero "01012"
    run $ Destroy (EnemyAttackSource eid) beravor
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 2

  it "reads the printed threat of an attacker that left play before its Forced effect resolves" . porosTest $ \_ -> do
    q <- stageB Quests.battleAtTheCrossings
    run $ PlaceTokens GameSource q Progress 5
    eid <- stage Enemies.southronSoldier
    beravor <- hero "01012"
    runOnly $ Destroy (EnemyAttackSource eid) beravor
    run $ Discard (EnemyTarget eid)
    enemyCodes `shouldReturn` []
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 2

  it "loses the game as Poros Garrison leaves play" . porosTest $ \pid -> do
    card <- genEncounterCard Objectives.porosGarrison
    run $ PutObjectiveAllyIntoPlayUnder pid card
    al <- fromJustNote "no garrison" <$> allyOf Objectives.porosGarrison
    run $ Discard (toTarget $ allyId al)
    getsGame (.outcome) `shouldReturn` Just Lost

  -- \* The Black Serpent

  it "has The Black Serpent take no damage, leave the staging area never, and get +X for the stage number" . porosTest $ \pid -> do
    _ <- stageB Quests.theDesertPath
    eid <- stage Enemies.theBlackSerpentCrossingsOfPoros
    getEnemyThreat eid `shouldReturn` 2
    run $ DealDamage GameSource (toTarget eid) 3
    getDamage (toTarget eid) `shouldReturn` 0
    run $ EngageEnemy pid eid
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  it "has The Black Serpent remove X progress from the main quest at the end of the encounter phase, or attack" . porosTest $ \pid -> do
    q <- stageB Quests.theDesertPath
    run $ PlaceTokens GameSource q Progress 3
    _ <- stage Enemies.theBlackSerpentCrossingsOfPoros
    run $ CheckWindows [mkWhen $ PhaseEnds EncounterPhase]
    chooseLabelStarting "$label.removeMainQuestProgress" pid
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 1

  -- \* The encounter cards

  it "has Desolate Land's threat be the most characters a player controls, and reveal a card of the largest set aside set" . porosTest $ \_ -> do
    _ <- setSetAside [Enemies.southronSoldier, Enemies.southronArcher, Quests.craftRemedy]
    lid <- stageLocation Locations.desolateLand
    getLocationThreat lid `shouldReturn` 3
    run $ TravelTo lid
    resolveRevealed
    enemyCodes >>= (`shouldSatisfy` any (`elem` map toCardCode [Enemies.southronSoldier, Enemies.southronArcher]))
    length <$> setAsideCodes `shouldReturn` 2

  it "has Harondor Waste place a card of the largest set aside set in the discard pile and shuffle it into the deck" . porosTest $ \_ -> do
    _ <- setSetAside [Enemies.southronSoldier, Enemies.southronArcher, Enemies.giantScorpion]
    _ <- setEncounterDeck []
    lid <- stageLocation Locations.harondorWaste
    run $ TravelTo lid
    setAsideCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.giantScorpion))
    length <$> setAsideCodes `shouldReturn` 2
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` \d -> length d == 1 && all (`elem` map toCardCode [Enemies.southronSoldier, Enemies.southronArcher]) d)

  it "has the first player choose between set aside sets tied for the most cards" . porosTest $ \pid -> do
    _ <- setSetAside [Enemies.giantScorpion, Enemies.southronSoldier]
    reveal [Treacheries.dangerCloseBehind]
    chooseLabel "$label.theCrossingsOfPoros.set.DesertCreatures" pid
    enemyCodes `shouldReturn` [toCardCode Enemies.giantScorpion]
    setAsideCodes `shouldReturn` [toCardCode Enemies.southronSoldier]

  it "has Danger Close Behind reveal a card of the largest set aside set" . porosTest $ \_ -> do
    _ <- setSetAside [Enemies.giantScorpion]
    reveal [Treacheries.dangerCloseBehind]
    enemyCodes `shouldReturn` [toCardCode Enemies.giantScorpion]
    setAsideCodes `shouldReturn` []

  it "has Mountains of Shadow raise each player's threat by the active location's printed quest points, or surge" . porosTest $ \pid -> do
    lid <- stageLocation Locations.harondorWaste
    run $ PlaceLocation lid ActiveLocation
    threat0 <- threatOf pid
    reveal [Treacheries.mountainsOfShadow]
    threatOf pid `shouldReturn` (threat0 + 3)

  it "has Mountains of Shadow surge with no active location" . porosTest $ \pid -> do
    threat0 <- threatOf pid
    reveal [Treacheries.mountainsOfShadow, Locations.harondorWaste]
    threatOf pid `shouldReturn` threat0
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "bars canceling 'when revealed' effects while Foothills of Mordor is in the staging area, and raises threat by 2 as it becomes active" . porosTest $ \pid -> do
    lid <- stageLocation Locations.foothillsOfMordor
    getModifiers GameTarget >>= (`shouldSatisfy` elem WhenRevealedCannotBeCanceled)
    threat0 <- threatOf pid
    run $ TravelTo lid
    threatOf pid `shouldReturn` (threat0 + 2)
    getModifiers GameTarget >>= (`shouldSatisfy` notElem WhenRevealedCannotBeCanceled)

  it "gives Shadowy Vale +3 threat while a player has 35 threat or more" . porosTest $ \pid -> do
    lid <- stageLocation Locations.shadowyVale
    getLocationThreat lid `shouldReturn` 2
    run $ RaiseThreat pid 35
    getLocationThreat lid `shouldReturn` 5

  it "bars reducing threat while Find a Way Down is in play, and reduces each player's threat by 3 as it is defeated" . porosTest $ \pid -> do
    _ <- putQuestStage Quests.theDesertPath
    run $ RaiseThreat pid 5
    reveal [Quests.findAWayDown, Locations.harondorWaste]
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotReduceThreat)
    threat0 <- threatOf pid
    side <- getsGame (toTarget . fromJustNote "no side quest" . find ((== toCardCode Quests.findAWayDown) . toCardCode) . toList . (.entities.quests))
    run $ PlaceTokens GameSource side Progress 6
    threatOf pid `shouldReturn` (threat0 - 3)

  it "has Southron Outrider place a random set aside Harad Soldiers enemy in the discard pile as it engages, or get +3 attack" . porosTest $ \pid -> do
    _ <- setSetAside [Enemies.southronSoldier]
    eid <- stage Enemies.southronOutrider
    run $ EngageEnemy pid eid
    chooseLabel "$label.southronOutrider.discard" pid
    setAsideCodes `shouldReturn` []
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.southronSoldier]

  it "has Uruk Prowler attack the first player when no Mordor Orcs enemy is set aside" . porosTest $ \pid -> do
    _ <- setSetAside []
    reveal [Enemies.urukProwler]
    -- the only choice is the attack, made at once
    getQuestion pid >>= (`shouldSatisfy` isJust)
    getsGame (length . (.entities.enemies)) `shouldReturn` 1

  it "has Desert Warg surge when its set aside enemies are gone" . porosTest $ \_ -> do
    _ <- setSetAside []
    reveal [Enemies.desertWarg, Locations.harondorWaste]
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  -- \* Wind from the Sea

  it "readies each committed hero as Wind from the Sea is revealed, and removes it from the game" . gameTestIn theCrossingsOfPoros windDeck $ \pid -> do
    wind <- fromJustNote "not set aside" . headMay . (.setAsideCards) <$> getPlayer pid
    _ <- setEncounterDeck []
    run $ ShuffleIntoEncounterDeck (CardIdTarget wind.id)
    beravor <- hero "01012"
    run $ CommitToQuest beravor
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    run RevealEncounterCard
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
    (.setAsideCards) <$> getPlayer pid `shouldReturn` []
    getEncounterDiscard `shouldReturn` []
 where
  windDeck = deckList testDeck.heroes ((toCardCode Events.windFromTheSea, 1) : testDeck.cards)
