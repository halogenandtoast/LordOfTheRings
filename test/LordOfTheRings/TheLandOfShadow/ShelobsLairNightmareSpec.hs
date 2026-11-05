module LordOfTheRings.TheLandOfShadow.ShelobsLairNightmareSpec (spec) where

import LordOfTheRings.Ability (isForcedAbility)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat, getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank, CannotBeTraveledTo, CannotExhaust))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Scenario (shelobsLair)
import LordOfTheRings.Scenario.Types (modeL, GameMode (NightmareMode))
import TestImport

lair :: (PlayerId -> GameT ()) -> IO ()
lair = gameTestIn shelobsLair testDeck

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn (overAttrs (modeL .~ NightmareMode) shelobsLair) testDeck

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
