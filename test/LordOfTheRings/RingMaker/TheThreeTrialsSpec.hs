module LordOfTheRings.RingMaker.TheThreeTrialsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (..), HasCardDef (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theThreeTrials)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Trait (Trait (Key))
import TestImport

trialsTest :: (PlayerId -> GameT ()) -> IO ()
trialsTest = gameTestIn theThreeTrials testDeck

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

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . (.victoryDisplay) <$> scenarioAttrs

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | Choose the stage 2 trial to advance to, out of the quest deck.
chooseTrial :: PlayerId -> CardDef -> GameT ()
chooseTrial pid def = do
  deck <- (.questDeck) <$> scenarioAttrs
  card <- maybe (error "no such trial") pure $ find ((== toCardCode def) . toCardCode) deck
  chooseTarget pid card
  resolveWhenRevealed pid

-- | Set the game up, through The Trials Begin, to the trial chosen.
startTrial :: PlayerId -> CardDef -> GameT ()
startTrial pid def = do
  flipFirstQuest pid
  resolveWhenRevealed pid
  chooseTrial pid def

-- | Claim the Key the first player is asked to, onto the hero.
claimOnto :: PlayerId -> CardCode -> GameT ()
claimOnto pid hero = chooseTarget pid =<< getHeroWithCode hero

-- | A Key claimed onto Beravor, out of the encounter deck.
heldKey :: PlayerId -> CardDef -> GameT ObjectiveId
heldKey pid def = do
  run . AddToStaging =<< genEncounterCard def
  claimOnto pid "01012"
  objectiveOf def

spec :: Spec
spec = describe "The Three Trials" do
  it "is a scenario the game can be dealt" . trialsTest $ \_ ->
    isJust (lookupScenario "the-three-trials") `shouldBe` True

  it "sets the Guardians, Keys, Barrows and Hallowed Circle aside, and lets the players choose a trial" . trialsTest $ \pid -> do
    flipFirstQuest pid
    aside <- map toCardCode . (.scenarioSetAside) <$> scenarioAttrs
    length aside `shouldBe` 10
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` all (`notElem` aside)
    resolveWhenRevealed pid
    chooseTrial pid Quests.theTrialOfIntuition
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.theTrialOfIntuition

  it "attaches the Guardian's Key to it in The Trial of Strength, and advances once the Key is claimed" . trialsTest $ \pid -> do
    startTrial pid Quests.theTrialOfStrength
    guardians <- getsGame (filter ((== Just 3) . cdVictoryPoints . toCardDef) . toList . (.entities.enemies))
    guardian <- maybe (error "no guardian") pure (headMay guardians)
    -- the Guardian engages the first player, its Key attached to it
    guardian.placement `shouldBe` EngagedWith pid
    heldKeys <- getsGame (toList . (.entities.objectives))
    map (.placement) heldKeys `shouldBe` [AttachedTo (toTarget guardian)]
    barrow <- getsGame (headMay . toList . (.entities.locations))
    -- Barrows cannot be the active location
    for_ barrow \b -> getModifiers (toTarget b) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ DealDamage GameSource (toTarget guardian) 20
    claimOnto pid "01012"
    codes <- victoryCodes
    codes `shouldSatisfy` elem (toCardCode guardian)
    for_ barrow \b -> codes `shouldSatisfy` elem (toCardCode b)
    -- the two trials left to choose from
    chooseTrial pid Quests.theTrialOfPerseverance
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.theTrialOfPerseverance

  it "keeps Guardians from damage and leaving play in The Trial of Perseverance until its Key is claimed" . trialsTest $ \pid -> do
    startTrial pid Quests.theTrialOfPerseverance
    [guardian] <- getsGame (toList . (.entities.enemies))
    [barrow] <- getsGame (toList . (.entities.locations))
    [key] <- getsGame (toList . (.entities.objectives))
    key.placement `shouldBe` AttachedTo (toTarget barrow)
    guardian.placement `shouldBe` EngagedWith pid
    run $ DealDamage GameSource (toTarget guardian) 3
    getDamage (toTarget guardian) `shouldReturn` 0
    run $ Discard (toTarget guardian)
    placementOf (toCardDef guardian) `shouldReturn` Just (EngagedWith pid)
    -- exploring the Barrow frees its Key, which the first player claims
    run $ PlaceTokens GameSource (toTarget barrow) Progress 10
    claimOnto pid "01007"
    victoryCodes >>= (`shouldSatisfy` elem (toCardCode guardian))

  it "discards encounter cards for progress in The Trial of Intuition, a Key among them claimed" . trialsTest $ \pid -> do
    startTrial pid Quests.theTrialOfIntuition
    key <- getsGame (find (member Key . cdCardTraits . toCardDef) . (.encounterDeck) . toAttrs . (.scenario))
    deck <- getEncounterDeck
    let deckSize = length deck
    -- the Key is among the bottom 10
    drop (deckSize - 10) deck `shouldSatisfy` \bottom -> isJust key && all (`elem` bottom) key
    quest <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource quest Progress 2
    length <$> getEncounterDeck `shouldReturn` deckSize - 2
    (.progress) <$> getActiveQuest `shouldReturn` 0
    _ <- setEncounterDeck . map toCardDef . maybeToList $ key
    run $ PlaceTokens GameSource quest Progress 1
    claimOnto pid "01012"
    -- the trial's Guardians and Barrows go to the victory display as it advances
    getsGame (length . toList . (.entities.enemies)) `shouldReturn` 0
    getsGame (length . toList . (.entities.locations)) `shouldReturn` 0
    chooseTrial pid Quests.theTrialOfStrength

  it "returns a Key discarded from the encounter deck to the staging area, where the first player claims it" . trialsTest $ \pid -> do
    run . AddToEncounterDiscard =<< traverse genEncounterCard [Objectives.keyOfTheWolf]
    claimOnto pid "01002"
    eventually <- objectiveOf Objectives.keyOfTheWolf
    (.placement) <$> getsGame (fromJustNote "key" . find ((== eventually) . toId) . toList . (.entities.objectives))
      >>= (`shouldSatisfy` \case AttachedTo (HeroTarget _) -> True; _ -> False)
    getEncounterDiscard `shouldReturn` []

  it "shuffles a Key dealt as a shadow card back into the encounter deck" . trialsTest $ \pid -> do
    spirit <- staged Enemies.spiritOfTheWild
    run $ EngageEnemy pid spirit
    _ <- setEncounterDeck [Objectives.keyOfTheRaven]
    run $ DealShadowCard spirit
    run $ EnemyAttack pid spirit
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Objectives.keyOfTheRaven]

  it "gives Spirit of the Wild +1 threat and attack for each Key the players control" . trialsTest $ \pid -> do
    spirit <- staged Enemies.spiritOfTheWild
    _ <- heldKey pid Objectives.keyOfTheBoar
    _ <- heldKey pid Objectives.keyOfTheWolf
    getEnemyThreat spirit `shouldReturn` 3
    getEnemyAttack spirit `shouldReturn` 3

  it "resets Wolf's Guardian's time counters once it attacks as the last is removed" . trialsTest $ \pid -> do
    wolf <- staged Enemies.wolfsGuardian
    run $ EngageEnemy pid wolf
    getTimeCounters (toTarget wolf) `shouldReturn` 3
    _ <- setEncounterDeck [Locations.grimFoothills]
    run $ RemoveTokens GameSource (toTarget wolf) TimeCounter 3
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    claimOnto pid "01002"
    passActions pid
    getTimeCounters (toTarget wolf) `shouldReturn` 3

  it "has Boar's Guardian make the engaged player discard an ally" . trialsTest $ \pid -> do
    boar <- staged Enemies.boarsGuardian
    run $ EngageEnemy pid boar
    _ <- putAllyIntoPlay pid Allies.gondorianSpearman
    run $ RemoveTokens GameSource (toTarget boar) TimeCounter 2
    getsGame (length . toList . (.entities.allies)) `shouldReturn` 0
    getTimeCounters (toTarget boar) `shouldReturn` 2

  it "removes time counters from each enemy with Wild Tenacity" . trialsTest $ \_ -> do
    raven <- staged Enemies.ravensGuardian
    _ <- setEncounterDeck [Treacheries.wildTenacity]
    run RevealEncounterCard
    resolveRevealed
    getTimeCounters (toTarget raven) `shouldReturn` 3

  it "places progress on Grim Foothills before the active location" . trialsTest $ \_ -> do
    active <- stagedLocation Locations.cursedForest
    run $ PlaceLocation active ActiveLocation
    _ <- stagedLocation Locations.grimFoothills
    run $ PlaceProgressOnQuest 4
    -- 3 explore Grim Foothills, and only the last reaches the active location
    placementOf Locations.grimFoothills `shouldReturn` Nothing
    getsGame (fmap (.progress) . find ((== active) . toId) . toList . (.entities.locations)) `shouldReturn` Just 1

  it "keeps the allies in play to 5 with Cave Barrow" . trialsTest $ \pid -> do
    _ <- stagedLocation Locations.caveBarrow
    replicateM_ 5 $ putAllyIntoPlay pid Allies.gondorianSpearman
    ally <- putAllyIntoPlay pid Allies.gondorianSpearman
    chooseTarget pid ally
    getsGame (length . toList . (.entities.allies)) `shouldReturn` 5

  it "travels to Hallowed Circle only by exhausting 3 Keys, and wins once it is explored" . trialsTest $ \pid -> do
    _ <- putQuestStage Quests.theAntleredCrownTheThreeTrials
    circle <- stagedLocation Locations.hallowedCircle
    heldKeys <- traverse (heldKey pid) [Objectives.keyOfTheBoar, Objectives.keyOfTheWolf]
    _ <- staged Enemies.wolfsGuardian
    -- the Guardian engages the player holding Key of the Wolf
    placementOf Enemies.wolfsGuardian `shouldReturn` Just (EngagedWith pid)
    getModifiers (toTarget circle) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    raven <- heldKey pid Objectives.keyOfTheRaven
    getModifiers (toTarget circle) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo circle
    -- each engaged Guardian attacks as the players travel there
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    claimOnto pid "01002"
    passActions pid
    for_ (raven : heldKeys) \k ->
      getsGame (fmap (.exhausted) . find ((== k) . toId) . toList . (.entities.objectives)) `shouldReturn` Just True
    run $ AddToVictoryDisplay (toTarget circle)
    quest <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource quest Progress 1
    outcome `shouldReturn` Just Won

  it "reveals the Guardians in the victory display at The Antlered Crown, each engaging its Key's holder" . trialsTest $ \pid -> do
    circle <- genEncounterCard Locations.hallowedCircle
    raven <- genEncounterCard Enemies.ravensGuardian
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioSetAside = [circle], scenarioVictoryDisplay = [EncounterCard raven]})
    _ <- heldKey pid Objectives.keyOfTheRaven
    quest <- putQuestStage Quests.theAntleredCrownTheThreeTrials
    run $ Flip quest
    resolveWhenRevealed pid
    placementOf Locations.hallowedCircle `shouldReturn` Just StagingArea
    placementOf Enemies.ravensGuardian `shouldReturn` Just (EngagedWith pid)
    victoryCodes `shouldReturn` []

  it "raises each player's threat by a Guardian's threat after it attacks at Stone Barrow" . trialsTest $ \pid -> do
    _ <- stagedLocation Locations.stoneBarrow
    raven <- staged Enemies.ravensGuardian
    run $ EngageEnemy pid raven
    threat <- (.threat) <$> getPlayer pid
    run $ EnemyAttack pid raven
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    claimOnto pid "01002"
    passActions pid
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3

  it "reveals a Guardian from the victory display with The Guardian's Fury when none is in play" . trialsTest $ \_ -> do
    boar <- genEncounterCard Enemies.boarsGuardian
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioVictoryDisplay = [EncounterCard boar]})
    _ <- setEncounterDeck [Treacheries.theGuardiansFury]
    run RevealEncounterCard
    resolveRevealed
    placementOf Enemies.boarsGuardian `shouldReturn` Just StagingArea
