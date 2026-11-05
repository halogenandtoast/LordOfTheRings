module LordOfTheRings.TheLandOfShadow.ShelobsLairNightmareSpec (spec) where

import LordOfTheRings.Ability (isForcedAbility)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat, getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank, CannotBeTraveledTo, CannotExhaust, CannotDiscardFromHand, CannotReady))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Scenario (shelobsLair)
import LordOfTheRings.Scenario.Types (modeL, GameMode (NightmareMode), ScenarioAttrs (scenarioSetAside))
import TestImport

lair :: (PlayerId -> GameT ()) -> IO ()
lair = gameTestIn shelobsLair testDeck

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn (overAttrs (modeL .~ NightmareMode) shelobsLair) testDeck

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

attack :: PlayerId -> EnemyId -> Target -> GameT ()
attack pid eid defender = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseTarget pid defender
  passActions pid
  resolveShadowsInOrder pid
  passActions pid

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations))

putSmeagol :: PlayerId -> GameT AllyId
putSmeagol pid = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.smeagolTheLandOfShadow
  fromJustNote "Smeagol" . headMay <$> select (AllyWithCardCode $ toCardCode Objectives.smeagolTheLandOfShadow)

resolveForced :: PlayerId -> GameT ()
resolveForced pid = getQuestion pid >>= \case
  Just q | any forcedChoice (questionChoices q) ->
    chooseOptionMatching "Forced ability" pid forcedChoice >> resolveForced pid
  _ -> pure ()
 where
  forcedChoice = \case
    AbilityLabel _ ability _ _ -> isForcedAbility ability
    _ -> False

spec :: Spec
spec = describe "Shelob's Lair Nightmare" do
  for_ [StagingArea, ActiveLocation] \placement ->
    it ("Foul-smelling Cave places a resource on Shelob when explored from " <> show placement) . lair $ \pid -> do
      spider <- stage Enemies.shelob
      lid <- stageLocation Locations.foulSmellingCave
      run $ PlaceLocation lid placement
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
      resolveForced pid
      (.resources) <$> getEnemy spider `shouldReturn` 1
      selectAny (LocationWithId lid) `shouldReturn` False

  it "Foul-smelling Cave's exploration is harmless without Shelob" . lair $ \pid -> do
    lid <- stageLocation Locations.foulSmellingCave
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
    resolveForced pid
    selectAny (LocationWithId lid) `shouldReturn` False

  it "discarding Foul-smelling Cave does not trigger its explored effect" . lair $ \_ -> do
    spider <- stage Enemies.shelob
    lid <- stageLocation Locations.foulSmellingCave
    run $ Discard GameSource (LocationTarget lid)
    (.resources) <$> getEnemy spider `shouldReturn` 0

  it "blanked Foul-smelling Cave does not place a resource when explored" . lair $ \pid -> do
    spider <- stage Enemies.shelob
    lid <- stageLocation Locations.foulSmellingCave
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
    resolveForced pid
    (.resources) <$> getEnemy spider `shouldReturn` 0

  for_ [(Enemies.forestSpider, 1), (Enemies.shelob, 2)] \(def, bonus) ->
    it ("Foul-smelling Cave's shadow gives the correct bonus to " <> show (toCardCode def)) . lair $ \pid -> do
      eid <- stage def
      attackBefore <- getEnemyAttack eid
      run . AttachShadowCard eid =<< genEncounterCard Locations.foulSmellingCave
      run $ ResolveShadow eid pid []
      getEnemyAttack eid `shouldReturn` (attackBefore + bonus)
      run $ EndEffects UntilEndOfAttack
      getEnemyAttack eid `shouldReturn` attackBefore

  it "Torech Ungol's staged copies stack attack and defense bonuses, including for Shelob entering later" . lair $ \_ -> do
    lid <- stageLocation Locations.torechUngol
    _ <- stageLocation Locations.torechUngol
    spider <- stage Enemies.shelob
    getEnemyAttack spider `shouldReturn` 6
    getEnemyDefense spider `shouldReturn` 6
    run $ PlaceLocation lid ActiveLocation
    getEnemyAttack spider `shouldReturn` 5
    getEnemyDefense spider `shouldReturn` 5
    run $ CreateEffect $ Effect GameSource (EnemyTarget spider) [Blank] UntilEndOfRound
    getEnemyAttack spider `shouldReturn` 5

  it "Torech Ungol requires a ready Smeagol and exhausts him to travel" . lair $ \pid -> do
    lid <- stageLocation Locations.torechUngol
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` StagingArea
    aid <- putSmeagol pid
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` ActiveLocation
    allyExhausted . toAttrs <$> getAlly aid `shouldReturn` True

  it "an exhausted Smeagol cannot pay Torech Ungol's travel cost" . lair $ \pid -> do
    aid <- putSmeagol pid
    run $ Exhaust (AllyTarget aid)
    lid <- stageLocation Locations.torechUngol
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` StagingArea

  it "blanking Torech Ungol removes both its travel cost and its bonus" . lair $ \_ -> do
    spider <- stage Enemies.shelob
    lid <- stageLocation Locations.torechUngol
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    getEnemyAttack spider `shouldReturn` 4
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` ActiveLocation

  it "Cobwebbed Tunnel boosts itself and other staged Underground locations, including later entrants" . lair $ \_ -> do
    tunnel <- stageLocation Locations.cobwebbedTunnel
    getLocationThreat tunnel `shouldReturn` 3
    getLocationQuestPoints tunnel `shouldReturn` 10
    cave <- stageLocation Locations.foulSmellingCave
    outside <- stageLocation Locations.woodedGlade
    getLocationThreat cave `shouldReturn` 5
    getLocationQuestPoints cave `shouldReturn` 6
    getLocationThreat outside `shouldReturn` 4
    run $ PlaceLocation cave ActiveLocation
    getLocationThreat cave `shouldReturn` 4
    getLocationQuestPoints cave `shouldReturn` 1
    run $ PlaceLocation tunnel ActiveLocation
    getLocationThreat tunnel `shouldReturn` 2
    getLocationQuestPoints tunnel `shouldReturn` 5

  it "two Cobwebbed Tunnels stack and their bonuses disappear as each leaves staging" . lair $ \_ -> do
    tunnel <- stageLocation Locations.cobwebbedTunnel
    other <- stageLocation Locations.cobwebbedTunnel
    getLocationThreat tunnel `shouldReturn` 4
    getLocationQuestPoints tunnel `shouldReturn` 15
    run $ Discard GameSource (LocationTarget other)
    getLocationThreat tunnel `shouldReturn` 3
    getLocationQuestPoints tunnel `shouldReturn` 10

  it "Cobwebbed Tunnel cannot take a player card's progress" . lair $ \_ -> do
    lid <- stageLocation Locations.cobwebbedTunnel
    hero <- toId <$> getHeroWithCode "01012"
    run $ PlaceTokens (HeroSource hero) (LocationTarget lid) Progress 2
    (.progress) <$> getLocation lid `shouldReturn` 0
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 2
    (.progress) <$> getLocation lid `shouldReturn` 2

  it "Cobwebbed Tunnel reveals an encounter card as a travel cost" . lair $ \_ -> do
    lid <- stageLocation Locations.cobwebbedTunnel
    _ <- setEncounterDeck [Locations.woodedGlade]
    run $ TravelTo lid
    resolveRevealed
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` ActiveLocation
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True

  it "Cobwebbed Tunnel's immunity prevents Thror's Key from blanking its text" . lair $ \pid -> do
    lid <- stageLocation Locations.cobwebbedTunnel
    _ <- putAttachmentOn pid Attachments.throrsKey (LocationTarget lid)
    getModifiers (LocationTarget lid) >>= (`shouldSatisfy` notElem Blank)
    getLocationQuestPoints lid `shouldReturn` 10

  it "Nightmare retains Shelob's unresolved shadows while discarding her resolved shadows and other enemies' cards" . nightmare $ \_ -> do
    spider <- stage Enemies.shelob
    other <- stage Enemies.forestSpider
    resolved <- genEncounterCard Locations.foulSmellingCave
    unresolved <- genEncounterCard Locations.woodedGlade
    run $ AttachShadowCard spider resolved
    run $ AttachShadowCard spider unresolved
    run $ RevealShadowCard spider resolved.id
    run . AttachShadowCard other =<< genEncounterCard Locations.woodedGlade
    run $ EndPhase CombatPhase
    map (.id) . (.shadowCards) <$> getEnemy spider `shouldReturn` [unresolved.id]
    (.shadowCards) <$> getEnemy other `shouldReturn` []
    map (.id) <$> getEncounterDiscard >>= (`shouldSatisfy` elem resolved.id)

  it "normal mode discards Shelob's unresolved shadow cards at the end of combat" . lair $ \_ -> do
    spider <- stage Enemies.shelob
    run . AttachShadowCard spider =<< genEncounterCard Locations.foulSmellingCave
    run $ EndPhase CombatPhase
    (.shadowCards) <$> getEnemy spider `shouldReturn` []

  it "Nightmare blocks other travel while Cleft of Cirith Ungol is staged, before charging a travel cost" . nightmare $ \pid -> do
    spider <- stage Enemies.shelob
    cleft <- stageLocation Locations.cleftOfCirithUngol
    aid <- putSmeagol pid
    lid <- stageLocation Locations.torechUngol
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` StagingArea
    allyExhausted . toAttrs <$> getAlly aid `shouldReturn` False
    run $ TravelTo cleft
    (.placement) . toAttrs <$> getLocation cleft `shouldReturn` ActiveLocation
    (.resources) <$> getEnemy spider `shouldReturn` 1
    getModifiers (LocationTarget lid) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

  it "normal mode does not block travel because Cleft of Cirith Ungol is staged" . lair $ \pid -> do
    _ <- stageLocation Locations.cleftOfCirithUngol
    _ <- putSmeagol pid
    lid <- stageLocation Locations.torechUngol
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` ActiveLocation


  it "Smeagol cannot pay Torech Ungol's travel cost when he cannot exhaust" . lair $ \pid -> do
    aid <- putSmeagol pid
    run $ CreateEffect $ Effect GameSource (AllyTarget aid) [CannotExhaust] UntilEndOfRound
    lid <- stageLocation Locations.torechUngol
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` StagingArea
    allyExhausted . toAttrs <$> getAlly aid `shouldReturn` False


  it "Orc of Minas Morgul lets you discard a random card without engaging or revealing another card" . lair $ \pid -> do
    _ <- putCardIntoHand pid Events.feint
    reveal [Enemies.orcOfMinasMorgul, Locations.woodedGlade]
    chooseLabel "$label.orcOfMinasMorgul.discard" pid
    (.hand) <$> getPlayer pid `shouldReturn` []
    enemies <- traverse getEnemy =<< select (EnemyWithCardCode $ toCardCode Enemies.orcOfMinasMorgul)
    map (.placement) enemies `shouldBe` [StagingArea]
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.woodedGlade]

  it "Orc of Minas Morgul engages you and reveals another card when your hand is empty" . lair $ \pid -> do
    reveal [Enemies.orcOfMinasMorgul, Locations.woodedGlade]
    enemies <- traverse getEnemy =<< select (EnemyWithCardCode $ toCardCode Enemies.orcOfMinasMorgul)
    map (.placement) enemies `shouldBe` [EngagedWith pid]
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True

  it "Orc of Minas Morgul cannot offer a discard forbidden by another effect" . lair $ \pid -> do
    card <- putCardIntoHand pid Events.feint
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
    reveal [Enemies.orcOfMinasMorgul, Locations.woodedGlade]
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    enemies <- traverse getEnemy =<< select (EnemyWithCardCode $ toCardCode Enemies.orcOfMinasMorgul)
    map (.placement) enemies `shouldBe` [EngagedWith pid]

  it "Orc of Minas Morgul's shadow adds two attack for the current attack" . lair $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.orcOfMinasMorgul
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 4
    run $ EndEffects UntilEndOfAttack
    getEnemyAttack eid `shouldReturn` 2

  it "Under-gate Sentry discards from the attacked player's hand after killing his character" . lair $ \pid -> do
    _ <- putCardIntoHand pid Events.feint
    eid <- stage Enemies.underGateSentry
    defender <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    attack pid eid defender
    resolveForced pid
    (.hand) <$> getPlayer pid `shouldReturn` []

  it "Under-gate Sentry does not discard after an attack with no destroyed character" . lair $ \pid -> do
    card <- putCardIntoHand pid Events.feint
    eid <- stage Enemies.underGateSentry
    defender <- toTarget <$> getHeroWithCode "01012"
    attack pid eid defender
    getDamage defender `shouldReturn` 3
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    asked <- getQuestion pid
    asked `shouldSatisfy` maybe True (not . any (\case AbilityLabel _ ab _ _ -> isForcedAbility ab; _ -> False) . questionChoices)

  it "blanking Under-gate Sentry prevents its forced discard" . lair $ \pid -> do
    card <- putCardIntoHand pid Events.feint
    eid <- stage Enemies.underGateSentry
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    defender <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    attack pid eid defender
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]

  it "Under-gate Sentry does not discard from either player when another player's Sentinel is destroyed" . twoPlayerTestIn shelobsLair testDeck $ \p1 p2 -> do
    ownCard <- putCardIntoHand p1 Events.feint
    attackedCard <- putCardIntoHand p2 Events.feint
    defender <- putAllyIntoPlay p1 Allies.wingedGuardian
    eid <- stage Enemies.underGateSentry
    run $ EnemyAttack p2 eid
    passActions p1
    passActions p2
    chooseLabel "$label.takeAttackUndefended" p2
    chooseTarget p1 defender
    passActions p1
    passActions p2
    resolveForced p1
    resolveForced p2
    map (.id) . (.hand) <$> getPlayer p1 `shouldReturn` [ownCard.id]
    map (.id) . (.hand) <$> getPlayer p2 `shouldReturn` [attackedCard.id]

  it "Under-gate Sentry's shadow offers a random discard or an exhausted character" . lair $ \pid -> do
    _ <- putCardIntoHand pid Events.feint
    hero <- toTarget <$> getHeroWithCode "01012"
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.underGateSentry
    run $ ResolveShadow eid pid []
    chooseLabel "$label.underGateSentry.exhaust" pid
    chooseTarget pid hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "Under-gate Sentry's shadow discards when no character can exhaust" . lair $ \pid -> do
    _ <- putCardIntoHand pid Events.feint
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes \hid -> run $ Exhaust (HeroTarget hid)
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.underGateSentry
    run $ ResolveShadow eid pid []
    (.hand) <$> getPlayer pid `shouldReturn` []

  it "Under-gate Sentry's shadow exhausts a character when hand discard is barred" . lair $ \pid -> do
    card <- putCardIntoHand pid Events.feint
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
    hero <- toTarget <$> getHeroWithCode "01012"
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.underGateSentry
    run $ ResolveShadow eid pid []
    chooseTarget pid hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]

  it "Impenetrable Dark removes one questing character per player and exhausts it" . twoPlayerTestIn shelobsLair testDeck $ \p1 p2 -> do
    h1 <- heroOf p1 "01012"
    h2 <- heroOf p2 "01012"
    run $ CommitToQuest h1
    run $ CommitToQuest h2
    run $ Ready h1
    run $ Ready h2
    reveal [Treacheries.impenetrableDark]
    for_ [p1, p2] \pid -> do
      asked <- getQuestion pid
      for_ asked \q -> when (any (\case TargetLabel {} -> True; _ -> False) $ questionChoices q) $
        chooseTarget pid (if pid == p1 then h1 else h2)
    selectCharacters CommittedHero CommittedAlly `shouldReturn` []
    for_ [p1, p2] \pid -> do
      heroTarget <- heroOf pid "01012"
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
      [heroExhausted (toAttrs h) | h <- heroes, toTarget h == heroTarget] `shouldBe` [True]

  it "Impenetrable Dark bars player readying through the round, including a later ally, but permits framework readying" . lair $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    reveal [Treacheries.impenetrableDark]
    run $ Exhaust hero
    runQueueT $ Lifted.ready hero
    void mainLoop
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    allyTarget <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ Exhaust allyTarget
    runQueueT $ Lifted.ready allyTarget
    void mainLoop
    aid <- fromJustNote "ally" . headMay <$> select (AllyControlledBy $ PlayerWithId pid)
    allyExhausted . toAttrs <$> getAlly aid `shouldReturn` True
    run $ Ready hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
    run $ EndEffects UntilEndOfRound
    runQueueT $ Lifted.ready allyTarget
    void mainLoop
    allyExhausted . toAttrs <$> getAlly aid `shouldReturn` False

  it "Impenetrable Dark's shadow exhausts the chosen character and bars all readying until the round ends" . lair $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.impenetrableDark
    run $ ResolveShadow eid pid []
    chooseTarget pid hero
    run $ Ready hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    run $ EndEffects UntilEndOfRound
    run $ Ready hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it "Growing Stench becomes Shelob's shadow and still surges without remaining in the discard pile" . lair $ \_ -> do
    eid <- stage Enemies.shelob
    reveal [Treacheries.growingStench, Locations.woodedGlade]
    map toCardCode . (.shadowCards) <$> getEnemy eid `shouldReturn` [toCardCode Treacheries.growingStench]
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.growingStench))
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True

  it "Growing Stench still surges when Shelob is out of play" . lair $ \_ -> do
    reveal [Treacheries.growingStench, Locations.woodedGlade]
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Treacheries.growingStench))
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True

  it "Growing Stench prevents readying after combat damage until its shadow card leaves" . lair $ \pid -> do
    putGame . (phaseL .~ CombatPhase) =<< getGame
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    hero <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.growingStench
    attack pid eid hero
    getModifiers hero >>= (`shouldSatisfy` elem CannotReady)
    run $ Ready hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    run $ DiscardShadowCards eid
    getModifiers hero >>= (`shouldSatisfy` notElem CannotReady)
    run $ Ready hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it "Growing Stench's readying restriction ends after an attack outside combat" . lair $ \pid -> do
    eid <- stage Enemies.forestSpider
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Treacheries.growingStench]
    runQueueT $ Lifted.immediateAttack pid eid
    void mainLoop
    passActions pid
    chooseTarget pid hero
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    getModifiers hero >>= (`shouldSatisfy` notElem CannotReady)
    run $ Ready hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it "Growing Stench places a resource on Shelob when her attack destroys a character, as printed" . lair $ \pid -> do
    eid <- stage Enemies.shelob
    defender <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.growingStench
    attack pid eid defender
    (.resources) <$> getEnemy eid `shouldReturn` 1

  it "Growing Stench does not place a resource when another enemy destroys a character" . lair $ \pid -> do
    spider <- stage Enemies.shelob
    eid <- stage Enemies.forestSpider
    defender <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.growingStench
    attack pid eid defender
    (.resources) <$> getEnemy spider `shouldReturn` 0

  it "Growing Stench does not place a resource when Shelob's defender survives" . lair $ \pid -> do
    eid <- stage Enemies.shelob
    hero <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.growingStench
    attack pid eid hero
    getDamage hero `shouldReturn` 2
    (.resources) <$> getEnemy eid `shouldReturn` 0

  it "Nightmare uses all seven new card types and exactly the printed removals" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    length deck `shouldBe` 35
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.morgulOrc, Locations.theUnderWay, Locations.deepDarkPassage, Treacheries.manyOpenings]
      `shouldBe` [0, 0, 0, 0]
    map copies [Locations.foulSmellingCave, Locations.torechUngol, Locations.cobwebbedTunnel, Enemies.orcOfMinasMorgul, Enemies.underGateSentry, Treacheries.impenetrableDark, Treacheries.growingStench]
      `shouldBe` [2, 3, 3, 3, 3, 3, 2]
    length <$> getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 14

  it "Nightmare setup preserves the standard starting locations and Smeagol" . nightmare $ \pid -> do
    run Setup
    chooseRingBearer pid (toCardCode Heroes.frodoBagginsTheLandOfShadow)
    chooseLabel "$label.keepHand" pid
    chooseOptionMatching "quest setup" pid (const True)
    selectAny (LocationWithCardCode (toCardCode Locations.morgulValeShelobsLair) <> IsActiveLocation) `shouldReturn` True
    selectAny (LocationWithCardCode (toCardCode Locations.smeagolsSecretWay) <> LocationInStagingArea) `shouldReturn` True
    selectAny (AllyWithCardCode (toCardCode Objectives.smeagolTheLandOfShadow) <> AllyControlledBy (PlayerWithId pid)) `shouldReturn` True


  it "Growing Stench stays dealt to Shelob when surge resets the deck, then remains unresolved after combat in Nightmare" . nightmare $ \_ -> do
    eid <- stage Enemies.shelob
    card <- genEncounterCard Locations.foulSmellingCave
    run $ AddToEncounterDiscard [card]
    putGame . (phaseL .~ QuestPhase) =<< getGame
    reveal [Treacheries.growingStench]
    map toCardCode . (.shadowCards) <$> getEnemy eid `shouldReturn` [toCardCode Treacheries.growingStench]
    selectAny (LocationWithCardCode $ toCardCode Locations.foulSmellingCave) `shouldReturn` True
    run $ EndPhase CombatPhase
    map toCardCode . (.shadowCards) <$> getEnemy eid `shouldReturn` [toCardCode Treacheries.growingStench]
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.growingStench))

  it "canceling Growing Stench's revelation preserves surge without dealing it to Shelob" . lair $ \pid -> do
    eid <- stage Enemies.shelob
    card <- putCardIntoHand pid Events.aTestOfWill
    hero <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource hero Resource 1
    _ <- setEncounterDeck [Treacheries.growingStench, Locations.woodedGlade]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    (.shadowCards) <$> getEnemy eid `shouldReturn` []
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True
