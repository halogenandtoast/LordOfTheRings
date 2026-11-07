module LordOfTheRings.Standalone.PrancingPonyLocationsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Scenario (murderAtThePrancingPony)
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers (Meta (..))
import LordOfTheRings.Standalone.PrancingPonyInvestigationSpec hiding (spec)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy, defendAttack, strongDefender)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

ponyTest :: (PlayerId -> GameT ()) -> IO ()
ponyTest body = gameTestIn murderAtThePrancingPony testDeck $ \pid -> run (DoStep 2 Setup) >> body pid

ponyTwo :: (PlayerId -> PlayerId -> GameT ()) -> IO ()
ponyTwo body = twoPlayerTestIn murderAtThePrancingPony testDeck $ \one two -> run (DoStep 2 Setup) >> body one two

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "missing location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations))

answerSearch :: Int -> GameT ()
answerSearch n | n <= 0 = error "Prancing Pony search did not finish"
answerSearch n = do
  asked <- getsGame (headMay . Map.keys . (.question))
  for_ asked $ \pid -> do
    chooseOptionMatching "resolve location search" pid (const True)
    answerSearch (n - 1)

retire :: CardDef -> GameT ()
retire def = stageLocation def >>= run . AddToVictoryDisplay . LocationTarget

spec :: Spec
spec = describe "Prancing Pony locations" do
  for_ [(Locations.billFernysHouse, Enemies.billFernyMurder), (Locations.staddleHidingHole, Enemies.greenwayFootpad), (Locations.combeStorehouse, Enemies.lawlessRuffian), (Locations.archetTradingPost, Enemies.breeLandInformant), (Locations.chetwoodCampsite, Enemies.chetwoodOutlaw)] $ \(def, enemyDef) -> do
    it ("requires and searches for its actual travel enemy: " <> show (toCardCode def)) . ponyTest $ \pid -> do
      lid <- stageLocation def
      getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      card : _ <- setEncounterDeck [enemyDef, Locations.oldForestRoad]
      getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      actor <- heroOf pid "01012"
      let source = case actor of HeroTarget hid -> HeroSource hid; _ -> GameSource
      immuneToSource source lid `shouldReturn` True
      run $ TravelTo lid
      answerSearch 20
      (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
      enemies <- getsGame (toList . (.entities.enemies))
      map ((.cardId) . toAttrs) enemies `shouldBe` [card.id]
      map ((.placement) . toAttrs) enemies `shouldBe` [StagingArea]
      getThreat pid `shouldReturn` if def == Locations.staddleHidingHole then 29 else 27

  it "can retrieve Bill Ferny from the victory display for his house's travel cost" . ponyTest $ \_ -> do
    eid <- stageEnemy Enemies.billFernyMurder
    run $ Destroy GameSource (EnemyTarget eid)
    lid <- stageLocation Locations.billFernysHouse
    _ <- setEncounterDeck [Locations.oldForestRoad]
    getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo lid
    answerSearch 20
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.billFernyMurder]

  it "makes suspects and their dealt shadows immune only while Bill Ferny's House is in the victory display" . ponyTest $ \pid -> do
    eid <- stageEnemy Enemies.toddTheTroll
    actor <- heroOf pid "01012"
    let source = case actor of HeroTarget hid -> HeroSource hid; _ -> GameSource
    immuneToSource source eid `shouldReturn` False
    retire Locations.billFernysHouse
    immuneToSource source eid `shouldReturn` True
    [shadow] <- setEncounterDeck [Locations.oldForestRoad]
    run $ DealShadowCard eid
    immuneToSource source (CardIdTarget shadow.id) `shouldReturn` True

  it "limits a suspect to attacks by its engaged player's characters with Staddle Hiding-hole in victory" . ponyTwo $ \one two -> do
    eid <- stageEnemy Enemies.toddTheTroll
    retire Locations.staddleHidingHole
    run $ EngageEnemy one eid
    mods <- getModifiers eid
    mods `shouldSatisfy` notElem (CannotBeAttackedByCharactersOf one)
    mods `shouldSatisfy` elem (CannotBeAttackedByCharactersOf two)

  it "makes a suspect attack immediately after engagement with Combe Storehouse in victory" . ponyTest $ \pid -> do
    eid <- stageEnemy Enemies.toddTheTroll
    retire Locations.combeStorehouse
    defender <- strongDefender pid
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run $ EngageEnemy pid eid
    defendAttack pid defender
    length <$> getEncounterDeck `shouldReturn` 1
    getDamage defender `shouldReturn` 0

  it "caps each suspect's damage at five each round with Archet Trading Post in victory" . ponyTest $ \_ -> do
    eid <- stageEnemy Enemies.toddTheTroll
    retire Locations.archetTradingPost
    run $ DealDamage GameSource (EnemyTarget eid) 7
    getDamage (EnemyTarget eid) `shouldReturn` 5
    run $ DealDamage GameSource (EnemyTarget eid) 2
    getDamage (EnemyTarget eid) `shouldReturn` 5
    run $ EndEffects UntilEndOfRound
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 6

  it "increases all three suspect stats with Chetwood Campsite in victory" . ponyTest $ \_ -> do
    eid <- stageEnemy Enemies.toddTheTroll
    retire Locations.chetwoodCampsite
    getEnemyThreat eid `shouldReturn` 3
    getEnemyAttack eid `shouldReturn` 8
    getEnemyDefense eid `shouldReturn` 3

  it "limits player-card threat reduction to one with Ramshackle Manor in play" . ponyTest $ \pid -> do
    _ <- stageLocation Locations.ramshackleManor
    _ <- playWith pid "01007" 3 Events.theGaladhrimsGreeting
    chooseLabel "$label.theGaladhrimsGreeting.one" pid
    getThreat pid `shouldReturn` 26

  it "charges each player's threat for traveling to Ramshackle Manor" . ponyTwo $ \one two -> do
    lid <- stageLocation Locations.ramshackleManor
    run $ TravelTo lid
    getThreat one `shouldReturn` 28
    getThreat two `shouldReturn` 28

  it "keeps staged enemies from taking damage, then discards to and stages its travel enemy without revelation" . ponyTest $ \pid -> do
    lid <- stageLocation Locations.darkAlleyMurder
    eid <- stageEnemy Enemies.lawlessRuffian
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 0
    run $ EngageEnemy pid eid
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1
    _ <- setEncounterDeck [Locations.oldForestRoad, Enemies.greenwayFootpad, Locations.oldForestRoad]
    run $ TravelTo lid
    getsGame (length . (.entities.enemies)) `shouldReturn` 2
    getThreat pid `shouldReturn` 27

  it "requires every player to pay a resource to travel to Market Square" . ponyTwo $ \one two -> do
    lid <- stageLocation Locations.marketSquareMurder
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    firstHero <- heroOf one "01007"
    run $ PlaceTokens GameSource firstHero Resource 1
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    secondHero <- heroOf two "01007"
    run $ PlaceTokens GameSource secondHero Resource 1
    getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo lid
    answerSearch 20
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    heroes <- getsGame (toList . (.entities.heroes))
    sum (map (.resources) heroes) `shouldBe` 0

  it "charges only the controller placing player-card progress in staging, once per placement" . ponyTwo $ \one two -> do
    _ <- stageLocation Locations.marketSquareMurder
    lid <- stageLocation Locations.oldWarehouse
    AllyTarget aid <- putAllyIntoPlay two Allies.northernTracker
    run $ PlaceTokens (AllySource aid) (LocationTarget lid) Progress 2
    getThreat one `shouldReturn` 27
    getThreat two `shouldReturn` 28
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
    getThreat two `shouldReturn` 28

  for_ [False, True] $ \additional ->
    it ("resolves Old Warehouse's Investigate choice before looking: " <> show additional) . ponyTwo $ \one two -> do
      initialize
      setClues =<< safeClues
      lid <- stageLocation Locations.oldWarehouse
      run $ PlaceLocation lid ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
      chooseLabel (if additional then "$label.prancingPony.lookAdditionalTwo" else "$label.prancingPony.reduceThreeEach") one
      length <$> shown one `shouldReturn` if additional then 3 else 1
      getThreat one `shouldReturn` if additional then 27 else 24
      getThreat two `shouldReturn` if additional then 27 else 24
      acceptLook one

  it "reveals the top encounter card as Old Warehouse's travel cost" . ponyTest $ \pid -> do
    lid <- stageLocation Locations.oldWarehouse
    _ <- setEncounterDeck [Treacheries.deadlyAccusations, Locations.oldForestRoad]
    run $ TravelTo lid
    resolveRevealed
    getThreat pid `shouldReturn` 28
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "requires every player to discard a random hand card to travel to Village Stable" . ponyTwo $ \one two -> do
    lid <- stageLocation Locations.villageStable
    _ <- putCardIntoHand one Allies.guardOfTheCitadel
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    _ <- putCardIntoHand two Allies.guardOfTheCitadel
    getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo lid
    length . (.hand) <$> getPlayer one `shouldReturn` 0
    length . (.hand) <$> getPlayer two `shouldReturn` 0

  it "moves a looked-at suspect from the Investigation deck to staging and retires the Stable exactly once" . ponyTest $ \pid -> do
    initialize
    clue <- fromJustNote "no suspect clue" . headMay . filter ((`elem` map toCardCode [Enemies.toddTheTroll, Enemies.susieTheSpider, Enemies.wolfPeltJake, Enemies.johnnyGoblinFingers, Enemies.oldOrcEyes]) . toCardCode) <$> safeClues
    setClues [clue]
    lid <- stageLocation Locations.villageStable
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
    acceptLook pid
    offersAbilityOf pid (LocationTarget lid) `shouldReturn` True
    chooseAbilityOf pid (LocationTarget lid)
    getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [clue.id]
    (.investigationDeck) <$> meta `shouldReturn` []
    victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
    length (filter ((== toCardCode Locations.villageStable) . toCardCode) victory) `shouldBe` 1

  it "forbids Dark Alley travel when neither the deck nor discard contains an enemy" . ponyTest $ \_ -> do
    lid <- stageLocation Locations.darkAlleyMurder
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.oldForestRoad
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea
    length <$> getEncounterDeck `shouldReturn` 1
    length <$> getEncounterDiscard `shouldReturn` 1

  it "can pay Dark Alley's travel cost with an enemy found only in the discard pile" . ponyTest $ \pid -> do
    lid <- stageLocation Locations.darkAlleyMurder
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.greenwayFootpad
    getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.greenwayFootpad]
    getThreat pid `shouldReturn` 27

  it "can travel to a blank Dark Alley without paying its printed cost" . ponyTest $ \_ -> do
    lid <- stageLocation Locations.darkAlleyMurder
    _ <- setEncounterDeck []
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
