module LordOfTheRings.Standalone.DolGuldurScenarioSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Modifier (ModifierType (AtEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getHero, getThreat, hasKeyword)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won, Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur)
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

stageDolGuldur :: Int -> GameT ObjectiveId
stageDolGuldur strength = do
  run . AddToStaging =<< genEncounterCard Objectives.dolGuldurAttack
  oid <- getsGame $ toId . fromJustNote "city missing" . headMay . filter ((== toCardCode Objectives.dolGuldurAttack) . toCardCode) . toList . (.entities.objectives)
  run $ DoStep 1 $ PlaceTokens GameSource (ObjectiveTarget oid) Resource strength
  pure oid

initialSetup :: GameT ()
initialSetup = do
  run $ DoStep 1 Setup
  run $ DoStep 6 Setup
  current <- getActiveQuest
  run $ PerformSetup $ toTarget current

stageFinal :: CardDef -> GameT Target
stageFinal def = do
  target <- putQuestStage def
  run $ Flip target
  resolveRevealed
  pure target

spec :: Spec
spec = describe "Attack on Dol Guldur scenario and quests" do
  it "sets up the standard city, Marauder, Power, and five set-aside cards" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    initialSetup
    cityStrength `shouldReturn` 10
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.dolGuldurMarauder]
    getsGame (length . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 5
    getsGame (sort . map toCardCode . toList . (.entities.objectives)) `shouldReturn` sort (map toCardCode [Objectives.dolGuldurAttack, Objectives.powerOfLorien])
    (.flipped) <$> getActiveQuest `shouldReturn` True
    length <$> getEncounterDeck `shouldReturn` 39

  it "scales standard setup with local player count" . twoPlayerTestIn attackOnDolGuldur testDeck $ \_ _ -> do
    initialSetup
    cityStrength `shouldReturn` 15
    getsGame (length . (.entities.enemies)) `shouldReturn` 2

  it "moves quest progress to Power while counting its three defensive rounds" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    initialSetup
    stage <- getActiveQuest
    run $ PlaceTokens GameSource (toTarget stage) Progress 7
    run $ UseCardAbility pid (QuestSource $ toId stage) 1 []
    (.progress) <$> getActiveQuest `shouldReturn` 0
    getsGame (map (.progress) . filter ((== toCardCode Objectives.powerOfLorien) . toCardCode) . toList . (.entities.objectives)) `shouldReturn` [7]
    run $ PlaceTokens GameSource (toTarget stage) Progress 4
    run $ UseCardAbility pid (QuestSource $ toId stage) 1 []
    getsGame (map (.progress) . filter ((== toCardCode Objectives.powerOfLorien) . toCardCode) . toList . (.entities.objectives)) `shouldReturn` [11]
    countTokens Resource . (.questTokens) . toAttrs <$> getActiveQuest `shouldReturn` 2

  it "pays the first stage's group threat cost before drawing and adding a chosen hero resource" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    initialSetup
    stage <- getActiveQuest
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid stage
    getThreat pid `shouldReturn` 1
    chooseLabel "$label.epic.thisGroup" pid
    hero <- heroOf pid "01012"
    chooseTarget pid hero
    getDamage hero `shouldReturn` 0
    held <- getPlayer pid
    length held.hand `shouldBe` 1
    heroCard <- case hero of HeroTarget hid -> getHero hid; _ -> error "expected hero"
    heroCard.resources `shouldBe` 1

  for_ [20, 30] $ \progress ->
    it ("requires 30 total progress to keep Power after taking the Host: " <> show progress) . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.powerOfLorien
      power <- getsGame $ toId . fromJustNote "missing power" . headMay . toList . (.entities.objectives)
      run $ PlaceTokens GameSource (ObjectiveTarget power) Progress progress
      run . SetAside =<< genEncounterCard Objectives.hostOfLorien
      _ <- stageFinal Quests.crossingTheAnduinDolGuldur
      chooseLabel "$label.dolGuldur.takeHost" pid
      when (progress == 30) $ chooseLabel "$label.dolGuldur.takePower" pid
      getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Objectives.hostOfLorien]
      getsGame (map (.placement) . toList . (.entities.objectives)) `shouldReturn`
        [AttachedTo (PlayerTarget pid) | progress == 30]
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.powerOfLorien | progress == 20]

  it "discards Power when the first player declines the Host, even with 30 progress" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.powerOfLorien
    power <- getsGame $ toId . fromJustNote "missing power" . headMay . toList . (.entities.objectives)
    run $ PlaceTokens GameSource (ObjectiveTarget power) Progress 30
    run . SetAside =<< genEncounterCard Objectives.hostOfLorien
    _ <- stageFinal Quests.crossingTheAnduinDolGuldur
    chooseLabel "$label.skip" pid
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    getsGame (length . (.entities.objectives)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.powerOfLorien]

  it "subtracts damage and allows repairs after reaching zero" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    city <- stageDolGuldur 3
    run $ DealDamage GameSource (ObjectiveTarget city) 8
    cityStrength `shouldReturn` 0
    getsGame (.outcome) `shouldReturn` Nothing
    run $ PlaceTokens GameSource (ObjectiveTarget city) Resource 4
    cityStrength `shouldReturn` 4
    getDamage (ObjectiveTarget city) `shouldReturn` 0

  it "removes Lieutenant indestructible only while city strength is zero" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    city <- stageDolGuldur 3
    eid <- stageEnemy Enemies.baneOfAmonLanc
    foe <- getEnemy eid
    hasKeyword Indestructible (EnemyTarget eid) (toCardDef foe) `shouldReturn` True
    run $ DealDamage GameSource (ObjectiveTarget city) 3
    hasKeyword Indestructible (EnemyTarget eid) (toCardDef foe) `shouldReturn` False
    run $ PlaceTokens GameSource (ObjectiveTarget city) Resource 2
    hasKeyword Indestructible (EnemyTarget eid) (toCardDef foe) `shouldReturn` True

  it "destroys a lethally damaged Lieutenant as soon as the city is breached" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    city <- stageDolGuldur 1
    eid <- stageEnemy Enemies.baneOfAmonLanc
    run $ DealDamage GameSource (EnemyTarget eid) 99
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    run $ DealDamage GameSource (ObjectiveTarget city) 1
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.baneOfAmonLanc]

  it "requires both a breached city and no surviving Lieutenant for round-end victory" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    _ <- stageDolGuldur 0
    eid <- stageEnemy Enemies.baneOfAmonLanc
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Nothing
    run $ EndEffects UntilEndOfRound
    run $ RemoveFromGame $ EnemyTarget eid
    getsGame (.outcome) `shouldReturn` Nothing
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Won

  it "counts destroyed enemies and explored locations but excludes simple discards" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    _ <- stageDolGuldur 20
    _ <- stageFinal Quests.dungeonsOfTheNecromancer
    eid <- stageEnemy Enemies.battleWargDolGuldur
    run $ Destroy GameSource $ EnemyTarget eid
    lid <- stageLocation Locations.dungeonDoor
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
    discarded <- stageEnemy Enemies.hostOfDolGuldur
    run $ Discard GameSource $ EnemyTarget discarded
    getsGame (roundSiege . getMeta . toAttrs . (.scenario)) `shouldReturn` 3
    run $ ScenarioSpecific "dolGuldur.resolveSiege" Null
    cityStrength `shouldReturn` 17
    getsGame (roundSiege . getMeta . toAttrs . (.scenario)) `shouldReturn` 0

  it "sets aside the old Lieutenant and adds the selected stage's physical Lieutenant" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    _ <- stageDolGuldur 20
    old <- stageEnemy Enemies.baneOfAmonLanc
    run $ PlaceTokens GameSource (EnemyTarget old) Damage 2
    sorcerer <- genEncounterCard Enemies.sorcererOfDolGuldurAttack
    run $ SetAside sorcerer
    _ <- stageFinal Quests.theTowerOfSorcery
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.sorcererOfDolGuldurAttack]
    getsGame (map (.id) . scenarioSetAside . toAttrs . (.scenario)) >>= (`shouldSatisfy` notElem sorcerer.id)
    getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.baneOfAmonLanc]

  for_ [4, 5, 10] $ \progress ->
    it ("resolves the Siege stage's mandatory and additional progress effects in order: " <> show progress) . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
      _ <- stageDolGuldur 20
      target <- stageFinal Quests.theSiegeOfDolGuldur
      run $ PlaceTokens GameSource target Progress progress
      run $ UseCardAbility pid (case target of QuestTarget qid -> QuestSource qid; _ -> GameSource) 1 []
      when (progress >= 5) $ chooseLabel "$label.dolGuldur.removeFive" pid
      when (progress >= 10) $ chooseLabel "$label.dolGuldur.payFive" pid
      cityStrength `shouldReturn` (if progress < 5 then 23 else if progress >= 10 then 17 else 20)
      (.progress) <$> getActiveQuest `shouldReturn` (if progress < 5 then 4 else 0)

  it "resolves the Battle stage's per-player enemy search without revelation or duplication on an empty-deck reset" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    _ <- stageDolGuldur 20
    target <- stageFinal Quests.battleUnderTheTrees
    cards <- setEncounterDeck [Locations.oldForestRoad, Enemies.kingSpider]
    run $ UseCardAbility pid (case target of QuestTarget qid -> QuestSource qid; _ -> GameSource) 1 []
    foes <- getsGame $ toList . (.entities.enemies)
    map toCardCode foes `shouldBe` [toCardCode Enemies.kingSpider]
    map (.placement) foes `shouldBe` [EngagedWith pid]
    traverse (getEnemyAttack . toId) foes `shouldReturn` [3]
    getsGame (map (heroExhausted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` replicate 3 False
    getQuestion pid `shouldReturn` Nothing
    remaining <- getEncounterDeck
    map toCardCode remaining `shouldBe` [toCardCode Locations.oldForestRoad]
    map (.id) remaining `shouldBe` take 1 (map (.id) cards)
    getEncounterDiscard `shouldReturn` []

  it "resolves the Tower's hand discard before offering its additional progress benefit" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    _ <- stageDolGuldur 20
    target <- stageFinal Quests.theTowerOfSorcery
    run $ DrawCards $ DrawingFrom pid 3 GameSource
    run $ PlaceTokens GameSource target Progress 5
    run $ UseCardAbility pid (case target of QuestTarget qid -> QuestSource qid; _ -> GameSource) 1 []
    chooseLabel "$label.dolGuldur.resolvePenalty" pid
    chooseOptionMatching "discard first card" pid (const True)
    chooseOptionMatching "discard second card" pid (const True)
    length . (.hand) <$> getPlayer pid `shouldReturn` 1
    chooseLabel "$label.dolGuldur.payFive" pid
    chooseOptionMatching "choose local player" pid (const True)
    chooseOptionMatching "search for a card" pid (const True)
    length . (.hand) <$> getPlayer pid `shouldReturn` 2
    (.progress) <$> getActiveQuest `shouldReturn` 0

  it "discards an ally for the Dungeons stage's unsatisfied mandatory progress requirement" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    _ <- stageDolGuldur 20
    target <- stageFinal Quests.dungeonsOfTheNecromancer
    allyTarget <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ UseCardAbility pid (case target of QuestTarget qid -> QuestSource qid; _ -> GameSource) 1 []
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` [toCardCode Allies.guardOfTheCitadel]
    case allyTarget of AllyTarget _ -> pure (); _ -> error "expected ally"

  for_ [False, True] $ \atRoundEnd ->
    it ("checks passive victory between the Lieutenant replacement sentences only at round end: " <> show atRoundEnd) . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
      _ <- stageDolGuldur 0
      _ <- stageEnemy Enemies.baneOfAmonLanc
      incoming <- genEncounterCard Enemies.sorcererOfDolGuldurAttack
      run $ SetAside incoming
      target <- putQuestStage Quests.theTowerOfSorcery
      when atRoundEnd $ run $ CreateEffect $ Effect GameSource GameTarget [AtEndOfRound] UntilEndOfRound
      getsGame (.outcome) `shouldReturn` Nothing
      run $ Flip target
      resolveRevealed
      if atRoundEnd then do
        getsGame (.outcome) `shouldReturn` Just Won
        getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` []
        getsGame (sort . map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn`
          sort [toCardCode Enemies.sorcererOfDolGuldurAttack, toCardCode Enemies.baneOfAmonLanc]
      else do
        getsGame (.outcome) `shouldReturn` Nothing
        getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.sorcererOfDolGuldurAttack]
        getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.baneOfAmonLanc]

  it "holds the first shared terminal result while city zero alone is nonterminal" . gameTest $ \_ -> do
    let initial = fromJustNote "invalid players" $ newDolGuldurEvent 3
        (breached, _) = applySharedDelta (SharedDelta UUID.nil (GroupOrdinal 0) DolGuldurCityStrength (-100)) initial
    dolGuldurCityStrength breached `shouldBe` Just 0
    dolGuldurOutcome breached `shouldBe` Nothing
    let winId = fromJustNote "invalid id" $ UUID.fromString "00000000-0000-0000-0000-000000000001"
        loseId = fromJustNote "invalid id" $ UUID.fromString "00000000-0000-0000-0000-000000000002"
        (won, _) = applySharedDelta (SharedDelta winId (GroupOrdinal 0) DolGuldurVictory 1) breached
        (final, effective) = applySharedDelta (SharedDelta loseId (GroupOrdinal 1) DolGuldurDefeat 1) won
    dolGuldurOutcome final `shouldBe` Just Won
    fmap deltaAmount effective `shouldBe` Just 0
    dolGuldurOutcome initial `shouldBe` Nothing
    let (lost, _) = applySharedDelta (SharedDelta loseId (GroupOrdinal 1) DolGuldurDefeat 1) initial
    dolGuldurOutcome lost `shouldBe` Just Lost
