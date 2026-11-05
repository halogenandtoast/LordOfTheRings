module LordOfTheRings.VengeanceOfMordor.DangerInDorwinionSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Effect (EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer), getEliminationLevel, getEnemy)
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (dangerInDorwinion, lookupScenario)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkAfter)
import TestImport

dorwinionTest :: (PlayerId -> GameT ()) -> IO ()
dorwinionTest = gameTestIn dangerInDorwinion testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

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
  getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

stageObjective :: CardDef -> GameT ObjectiveId
stageObjective def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

objectiveDamage :: ObjectiveId -> GameT Int
objectiveDamage oid = getsGame (maybe 0 (countTokens Damage . (.objectiveTokens) . toAttrs) . lookup oid . (.entities.objectives))

-- | A stage put into play on its B side, with the stages given after it in the quest deck.
stageB :: CardDef -> [CardDef] -> GameT Target
stageB def rest = do
  q <- putQuestStage def
  later <- traverse genEncounterCard rest
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) . (scenarioL %~ overAttrs (questDeckL .~ later)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

questResources :: GameT Int
questResources = countTokens Resource . (.questTokens) . toAttrs <$> getActiveQuest

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

-- | The cards set aside out of play, their codes.
setAsideCodes :: GameT [CardCode]
setAsideCodes = sort . map toCardCode . scenarioSetAside <$> scenarioAttrs

setAside :: [CardDef] -> GameT ()
setAside defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

-- | The player's deck, with these cards on top.
stackDeck :: PlayerId -> [CardDef] -> GameT ()
stackDeck pid defs = do
  cards <- traverse (`genPlayerCard` Just pid) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (map PlayerCard cards <>)

handTypes :: PlayerId -> GameT [CardType]
handTypes pid = map (.cardType) . (.hand) <$> getPlayer pid

discardCodes :: PlayerId -> GameT [CardCode]
discardCodes pid = map toCardCode . (.discard) <$> getPlayer pid

-- | The enemy attacks the player, the defender declared, actions passed.
attackDefendedBy :: PlayerId -> EnemyId -> Target -> GameT ()
attackDefendedBy pid eid defender = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseTarget pid defender
  passActions pid

cultistDefs :: [CardDef]
cultistDefs = [Enemies.priestOfSauron, Enemies.theGovernor, Enemies.blackNumenoreanASitE]

plotDefs :: [CardDef]
plotDefs = [Objectives.bribes, Objectives.submitOrDie, Objectives.sauronWorship]

spec :: Spec
spec = describe "Danger in Dorwinion" do
  it "is a scenario the game can be dealt" . dorwinionTest $ \_ ->
    isJust (lookupScenario "danger-in-dorwinion") `shouldBe` True

  -- \* Setup

  it "sets the Plot objectives and Cultist enemies aside, adds Side Street to the staging area, and turns stage 1 over" . dorwinionTest $ \pid -> do
    flipFirstQuest pid
    setAsideCodes `shouldReturn` sort (map toCardCode $ cultistDefs <> plotDefs)
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.sideStreet]
    deck <- map toCardCode . scenarioEncounterDeck <$> scenarioAttrs
    any (`elem` map toCardCode (cultistDefs <> plotDefs)) deck `shouldBe` False
    -- Danger in Dorwinion's own: 3 Evidence of the Cult, 3 Hillside Manor, 2 each of the
    -- treacheries; one Side Street went to the staging area
    let copies d = length (filter (== toCardCode d) deck)
    map copies [Objectives.evidenceOfTheCult, Locations.hillsideManor, Treacheries.disappearances, Treacheries.secretCultist, Locations.sideStreet]
      `shouldBe` [3, 3, 2, 2, 1]
    activeQuestCode `shouldReturn` toCardCode Quests.aCityInTurmoil
    (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True
    map toCardCode . (.questDeck) <$> scenarioAttrs `shouldReturn` map toCardCode [Quests.theShadowOfMordor, Quests.leaderOfTheCult]

  it "has each other player add a location with a different title" . twoPlayerTestIn dangerInDorwinion testDeck $ \p1 p2 -> do
    run Setup
    replicateM_ 2 do
      asked <- filterM (fmap isJust . getQuestion) [p1, p2]
      for_ (take 1 asked) $ chooseLabel "$label.keepHand"
    lead <- getLead
    let other = if lead == p1 then p2 else p1
    chooseOptionMatching "flip the quest" lead (const True)
    -- the other player is offered no Side Street
    Just q <- getQuestion other
    let offered = [cid | TargetLabel (CardIdTarget cid) _ <- questionChoices q]
    deck <- scenarioEncounterDeck <$> scenarioAttrs
    any (\c -> c.id `elem` offered && toCardCode c == toCardCode Locations.sideStreet) deck `shouldBe` False
    chooseTarget other (CardIdTarget $ fromJustNote "nothing offered" $ headMay offered)
    codes <- getsGame (map toCardCode . toList . (.entities.locations))
    length codes `shouldBe` 2
    toCardCode Locations.sideStreet `elem` codes `shouldBe` True
    length (ordNub codes) `shouldBe` 2

  it "removes the copies easy mode leaves out" . gameTestIn (overAttrs (modeL .~ EasyMode) dangerInDorwinion) testDeck $ \pid -> do
    flipFirstQuest pid
    deck <- map toCardCode . scenarioEncounterDeck <$> scenarioAttrs
    let copies d = length (filter (== toCardCode d) deck)
    map copies [Treacheries.disappearances, Treacheries.secretCultist, Objectives.evidenceOfTheCult, Locations.hillsideManor]
      `shouldBe` [1, 1, 3, 3]

  -- \* A City in Turmoil

  it "places a resource on A City in Turmoil as the active location is explored, and is not defeated with fewer than 2" . dorwinionTest $ \_ -> do
    q <- stageB Quests.aCityInTurmoil [Quests.theShadowOfMordor]
    run $ PlaceTokens GameSource q Progress 5
    activeQuestCode `shouldReturn` toCardCode Quests.aCityInTurmoil
    manor <- activeLocation Locations.hillsideManor
    run $ PlaceTokens GameSource (LocationTarget manor) Progress 4
    questResources `shouldReturn` 1
    activeQuestCode `shouldReturn` toCardCode Quests.aCityInTurmoil
    manor' <- activeLocation Locations.hillsideManor
    run $ PlaceTokens GameSource (LocationTarget manor') Progress 4
    activeQuestCode `shouldReturn` toCardCode Quests.theShadowOfMordor

  -- \* The Shadow of Mordor

  it "adds a random set-aside Plot objective and reveals 1 encounter card per player" . dorwinionTest $ \pid -> do
    setAside plotDefs
    _ <- setEncounterDeck [Locations.hillsideManor]
    _ <- putQuestStage Quests.theShadowOfMordor
    run . Flip . toTarget =<< getActiveQuest
    resolveWhenRevealed pid
    objectives <- getsGame (map toCardCode . toList . (.entities.objectives))
    length objectives `shouldBe` 1
    all (`elem` map toCardCode plotDefs) objectives `shouldBe` True
    length <$> setAsideCodes `shouldReturn` 2
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.hillsideManor]

  it "reveals an encounter card and places a resource as the active location is explored, defeated with 3 more resources than players" . dorwinionTest $ \_ -> do
    _ <- stageB Quests.theShadowOfMordor [Quests.leaderOfTheCult]
    let explore = do
          _ <- setEncounterDeck [Locations.hillsideManor]
          lid <- activeLocation Locations.sideStreet
          run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
    replicateM_ 3 explore
    questResources `shouldReturn` 3
    getsGame (length . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations)) `shouldReturn` 3
    activeQuestCode `shouldReturn` toCardCode Quests.theShadowOfMordor
    explore
    activeQuestCode `shouldReturn` toCardCode Quests.leaderOfTheCult

  -- \* Leader of the Cult

  it "adds a random set-aside Cultist, and with one player discards no enemies" . dorwinionTest $ \pid -> do
    setAside cultistDefs
    _ <- setEncounterDeck [Enemies.servantOfSauron]
    _ <- putQuestStage Quests.leaderOfTheCult
    run . Flip . toTarget =<< getActiveQuest
    resolveWhenRevealed pid
    codes <- enemyCodes
    length codes `shouldBe` 1
    all (`elem` map toCardCode cultistDefs) codes `shouldBe` True
    map toCardCode . scenarioEncounterDeck <$> scenarioAttrs `shouldReturn` [toCardCode Enemies.servantOfSauron]

  it "discards until one less enemy than the players is discarded, with two players" . twoPlayerTestIn dangerInDorwinion testDeck $ \p1 _ -> do
    setAside [Enemies.priestOfSauron]
    _ <- setEncounterDeck [Locations.hillsideManor, Enemies.servantOfSauron, Enemies.rhunBackstabber]
    _ <- putQuestStage Quests.leaderOfTheCult
    run . Flip . toTarget =<< getActiveQuest
    resolveWhenRevealed p1
    -- the deck is shuffled with the discard pile: either enemy may come first
    codes <- enemyCodes
    length codes `shouldBe` 2
    toCardCode Enemies.priestOfSauron `elem` codes `shouldBe` True

  it "caps the Cultist's damage each round at the active location's printed quest points, and wins as it is defeated" . dorwinionTest $ \_ -> do
    _ <- stageB Quests.leaderOfTheCult []
    governor <- stage Enemies.theGovernor
    -- no active location: no damage
    run $ PlaceTokens GameSource (EnemyTarget governor) Damage 3
    (.damage) <$> getEnemy governor `shouldReturn` 0
    _ <- activeLocation Locations.sideStreet
    run $ PlaceTokens GameSource (EnemyTarget governor) Damage 2
    run $ PlaceTokens GameSource (EnemyTarget governor) Damage 2
    (.damage) <$> getEnemy governor `shouldReturn` 3
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource (EnemyTarget governor) Damage 3
    (.damage) <$> getEnemy governor `shouldReturn` 6
    run $ Destroy GameSource (EnemyTarget governor)
    getsGame (.outcome) `shouldReturn` Just Won

  -- \* Cultists

  it "makes Priest of Sauron immune and not optionally engaged, and raises each threat by 2 as his attack destroys an ally" . dorwinionTest $ \pid -> do
    priest <- stage Enemies.priestOfSauron
    getModifiers (EnemyTarget priest) >>= (`shouldSatisfy` \ms -> ImmuneToPlayerCardEffects `elem` ms && CannotBeOptionallyEngagedBy AnyPlayer `elem` ms)
    run $ EngageEnemy pid priest
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    threat0 <- threatOf pid
    inPhase CombatPhase
    attackDefendedBy pid priest ally
    getsGame (null . (.entities.allies)) `shouldReturn` True
    threatOf pid `shouldReturn` threat0 + 2

  it "has The Governor engage the first player and attack after a location is revealed" . dorwinionTest $ \pid -> do
    governor <- stage Enemies.theGovernor
    reveal [Locations.hillsideManor]
    (.placement) <$> getEnemy governor `shouldReturn` EngagedWith pid
    getQuestion pid >>= (`shouldSatisfy` isJust)

  it "keeps Black Númenórean's shadow cards with an effect, and resolves them again as he attacks" . dorwinionTest $ \pid -> do
    numenorean <- stage Enemies.blackNumenoreanASitE
    run $ EngageEnemy pid numenorean
    _ <- setEncounterDeck [Treacheries.disappearances, Locations.hillsideManor]
    run $ DealShadowCard numenorean
    run $ DealShadowCard numenorean
    eowyn <- hero "01007"
    beravor <- hero "01012"
    stone <- putAttachmentOn pid Attachments.celebriansStone eowyn
    inPhase CombatPhase
    attackDefendedBy pid numenorean eowyn
    resolveShadowsInOrder pid
    -- Disappearances' shadow: discard an attachment you control
    chooseTarget pid stone
    passActions pid
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    run $ EndPhase CombatPhase
    map toCardCode . (.shadowCards) <$> getEnemy numenorean `shouldReturn` [toCardCode Treacheries.disappearances]
    stone' <- putAttachmentOn pid Attachments.celebriansStone beravor
    _ <- setEncounterDeck []
    attackDefendedBy pid numenorean beravor
    resolveShadowsInOrder pid
    chooseTarget pid stone'
    getsGame (length . (.entities.attachments)) `shouldReturn` 0

  -- \* Plots

  it "adds a threat to the staging area for each resource on Bribes, and places one per event discarded as the active location is explored" . dorwinionTest $ \pid -> do
    bribes <- stageObjective Objectives.bribes
    stackDeck pid [Allies.guardOfTheCitadel, eventDef, eventDef]
    manor <- activeLocation Locations.hillsideManor
    threat0 <- getStagingThreat
    run $ PlaceTokens GameSource (LocationTarget manor) Progress 4
    -- Hillside Manor's Threat is 2: the ally and one event are discarded
    getsGame (maybe 0 (.resources) . lookup bribes . (.entities.objectives)) `shouldReturn` 1
    sort <$> discardCodes pid `shouldReturn` sort [toCardCode eventDef, toCardCode Allies.guardOfTheCitadel]
    getStagingThreat `shouldReturn` threat0 + 1

  it "gives non-unique enemies +1 attack with Submit or Die, and loses the game at 4 more damage than players" . dorwinionTest $ \pid -> do
    submit <- stageObjective Objectives.submitOrDie
    servant <- stage Enemies.servantOfSauron
    priest <- stage Enemies.priestOfSauron
    getEnemyAttack servant `shouldReturn` 3
    getEnemyAttack priest `shouldReturn` 6
    run $ EngageEnemy pid servant
    inPhase CombatPhase
    for_ [1 .. 5 :: Int] \n -> do
      ally <- putAllyIntoPlay pid Allies.snowbournScout
      attackDefendedBy pid servant ally
      when (n < 5) $ objectiveDamage submit `shouldReturn` n
    getsGame (.outcome) `shouldReturn` Just Lost

  it "enters play with 1 damage on Sauron Worship, lowers the elimination level by each, and places 1 at the end of the round" . dorwinionTest $ \pid -> do
    worship <- stageObjective Objectives.sauronWorship
    objectiveDamage worship `shouldReturn` 1
    getEliminationLevel pid `shouldReturn` 49
    run $ CheckWindows [mkAfter RoundEnds]
    objectiveDamage worship `shouldReturn` 2
    getEliminationLevel pid `shouldReturn` 48

  -- \* Evidence of the Cult

  it "is guarded by an enemy that gets +1 Threat, Attack and Defense, and goes to the victory display with a resource on the main quest once free" . dorwinionTest $ \_ -> do
    _ <- putQuestStage Quests.aCityInTurmoil
    _ <- setEncounterDeck [Objectives.evidenceOfTheCult, Treacheries.disappearances, Enemies.servantOfSauron]
    run RevealEncounterCard
    resolveRevealed
    servant <- enemyOf Enemies.servantOfSauron
    getEnemyThreat servant `shouldReturn` 2
    getEnemyAttack servant `shouldReturn` 3
    getEnemyDefense servant `shouldReturn` 2
    run $ Destroy GameSource (EnemyTarget servant)
    victoryCodes >>= (`shouldSatisfy` elem (toCardCode Objectives.evidenceOfTheCult))
    questResources `shouldReturn` 1

  -- \* Hillside Manor

  it "limits threat reduction by player cards to 1 each round in the staging area, and discards a random card from each hand as it is traveled to" . dorwinionTest $ \pid -> do
    manor <- stageLocation Locations.hillsideManor
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (PlayerCardThreatReductionLimit 1))
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    hand0 <- length . (.hand) <$> getPlayer pid
    run $ TravelTo manor
    length . (.hand) <$> getPlayer pid `shouldReturn` hand0 - 1

  -- \* Treacheries

  it "has Disappearances discard each ally in hand, and gains doomed 2 if none is" . dorwinionTest $ \pid -> do
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    threat0 <- threatOf pid
    reveal [Treacheries.disappearances]
    handTypes pid >>= (`shouldSatisfy` notElem AllyType)
    threatOf pid `shouldReturn` threat0
    reveal [Treacheries.disappearances]
    threatOf pid `shouldReturn` threat0 + 2

  it "turns the highest attack ally into an enemy engaged with its owner, its damage kept, and discards it with Secret Cultist" . dorwinionTest $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    son <- putAllyIntoPlay pid Allies.sonOfArnor
    run $ PlaceTokens GameSource son Damage 1
    reveal [Treacheries.secretCultist]
    getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Allies.guardOfTheCitadel]
    turned <- enemyOf Allies.sonOfArnor
    e <- getEnemy turned
    e.placement `shouldBe` EngagedWith pid
    e.damage `shouldBe` 1
    getEnemyAttack turned `shouldReturn` 2
    getsGame (map (.placement) . toList . (.entities.treacheries)) `shouldReturn` [AttachedTo (EnemyTarget turned)]
    -- his threat elimination stays, as an ally turned is no hero side
    run . Discard GameSource . toTarget . fromJustNote "no Secret Cultist" . headMay . toList =<< getsGame (.entities.treacheries)
    enemyCodes `shouldReturn` []
    discardCodes pid >>= (`shouldSatisfy` elem (toCardCode Allies.sonOfArnor))

  it "puts the ally Secret Cultist turned in its owner's discard pile as it is destroyed" . dorwinionTest $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.sonOfArnor
    reveal [Treacheries.secretCultist]
    turned <- enemyOf Allies.sonOfArnor
    run $ Destroy GameSource (EnemyTarget turned)
    enemyCodes `shouldReturn` []
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    discardCodes pid >>= (`shouldSatisfy` elem (toCardCode Allies.sonOfArnor))
    getsGame (.outcome) `shouldReturn` Nothing
 where
  eventDef = Events.everVigilant
