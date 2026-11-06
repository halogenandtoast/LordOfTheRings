module LordOfTheRings.DarkOfMirkwood.EncounterSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.EncounterSet (EncounterSet (TheOath, TheGoblins, TheCavesOfNibinDum))
import LordOfTheRings.Scenario (theOath, theCavesOfNibinDum)
import LordOfTheRings.Scenario.Runner (gatherEncounterSets)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioMode), GameMode (EasyMode))
import LordOfTheRings.Card.Code (CardCode (..))
import LordOfTheRings.Card (lookupCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getLocationThreat)
import LordOfTheRings.Helpers.Window (passesCriteria, canPay)
import LordOfTheRings.Ability.Types (AbilityCost (DiscardFromHandCost))
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Criteria (Criterion (ReadyObjectiveWithCardCode, CardInEncounterDeckOrDiscard))
import LordOfTheRings.Matcher (CardMatcher (CardWithTrait), getConditionAttachments)
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo, CannotTakeDamage))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Trait (Trait (Spider))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "missing enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "missing location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

printed :: Int -> CardCode
printed n = CardCode $ "dark-of-mirkwood-" <> pack (replicate (3 - length (show n)) '0' <> show n)

pureIt :: String -> IO () -> Spec
pureIt = it

spec :: Spec
spec = describe "The Dark of Mirkwood encounter cards" do
  pureIt "registers all 51 printed cards, including campaign boons and burdens" $
    for_ [1 .. 51] $ \n -> lookupCardDef (printed n) `shouldSatisfy` isJust

  it "builds all 33 Oath cards and all 36 Caves cards from the printed encounter sets" . gameTest $ \_ -> do
    oath <- gatherEncounterSets (toAttrs theOath) [TheOath, TheGoblins]
    caves <- gatherEncounterSets (toAttrs theCavesOfNibinDum) [TheCavesOfNibinDum, TheGoblins]
    let codes = Map.fromListWith (+) . map (\c -> (toCardCode c, 1 :: Int))
    codes oath `shouldBe` Map.fromList [(printed n, q) | (n,q) <- [(8,2),(9,2),(10,2),(11,2),(12,2),(13,2),(14,2),(15,1),(16,2),(17,2),(18,1),(19,1),(20,2),(21,2),(22,2),(23,1),(24,1),(25,2),(26,2)] :: [(Int,Int)]]
    codes caves `shouldBe` Map.fromList [(printed n, q) | (n,q) <- [(19,1),(20,2),(21,2),(22,2),(23,1),(24,1),(25,2),(26,2),(27,1),(28,1),(29,1),(30,2),(31,2),(32,2),(33,2),(34,2),(35,2),(36,2),(37,1),(38,2),(39,2),(40,1)] :: [(Int,Int)]]

  it "removes only this printing's orange-ring cards in easy mode" . gameTest $ \_ -> do
    let oathAttrs = (toAttrs theOath) {scenarioMode = EasyMode}
        cavesAttrs = (toAttrs theCavesOfNibinDum) {scenarioMode = EasyMode}
    oath <- gatherEncounterSets oathAttrs [TheOath, TheGoblins]
    caves <- gatherEncounterSets cavesAttrs [TheCavesOfNibinDum, TheGoblins]
    length oath `shouldBe` 25
    length caves `shouldBe` 28
    map toCardCode oath `shouldSatisfy` all (`notElem` ["dark-of-mirkwood-008", "dark-of-mirkwood-011", "dark-of-mirkwood-018", "dark-of-mirkwood-021", "dark-of-mirkwood-024"])
    map toCardCode caves `shouldSatisfy` all (`notElem` ["dark-of-mirkwood-021", "dark-of-mirkwood-024", "dark-of-mirkwood-030", "dark-of-mirkwood-034", "dark-of-mirkwood-040"])

  it "exhausts a character when Great Spider engages" . gameTest $ \pid -> do
    stage Enemies.greatSpider
    spider <- enemyOf Enemies.greatSpider
    run $ EngageEnemy pid spider
    hero <- getHeroWithCode "01012"
    chooseTarget pid hero
    (heroExhausted . toAttrs) <$> getHeroWithCode "01012" `shouldReturn` True

  it "puts a discarded Goblin into play engaged after Wild Wargs engages" . gameTest $ \pid -> do
    stage Enemies.wildWargsDarkOfMirkwood
    wargs <- enemyOf Enemies.wildWargsDarkOfMirkwood
    _ <- setEncounterDeck [Enemies.goblinSniperDarkOfMirkwood]
    run $ EngageEnemy pid wargs
    sniper <- enemyOf Enemies.goblinSniperDarkOfMirkwood
    getsGame (fmap (.placement) . lookup sniper . (.entities.enemies)) `shouldReturn` Just (EngagedWith pid)
    getEncounterDiscard `shouldReturn` []

  it "sets Tangled Grove's threat from staged locations and bars its unpaid travel cost" . gameTest $ \_ -> do
    stage Locations.tangledGrove
    grove <- locationOf Locations.tangledGrove
    getLocationThreat grove `shouldReturn` 1
    stage Locations.forestGateDarkOfMirkwood
    getLocationThreat grove `shouldReturn` 2
    getModifiers grove >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  it "limits total quest progress each round while Collapsed Mine is staged" . gameTest $ \_ -> do
    quest <- putQuestStage Quests.theDarkOfNibinDum
    stage Locations.collapsedMine
    run $ PlaceTokens GameSource quest Progress 3
    run $ PlaceTokens GameSource quest Progress 3
    (.progress) <$> getActiveQuest `shouldReturn` 4
    mine <- locationOf Locations.collapsedMine
    run $ PlaceLocation mine ActiveLocation
    run $ PlaceTokens GameSource quest Progress 2
    (.progress) <$> getActiveQuest `shouldReturn` 6

  it "scales Goblin Chieftain's combat stats and damage immunity with the quest" . gameTest $ \_ -> do
    quest <- putQuestStage Quests.oathkeepers
    run $ Flip quest
    resolveRevealed
    stage Enemies.goblinChieftain
    chief <- enemyOf Enemies.goblinChieftain
    getEnemyAttack chief `shouldReturn` 4
    getEnemyDefense chief `shouldReturn` 4
    getModifiers chief >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ PlaceTokens GameSource quest Progress 8
    getModifiers chief >>= (`shouldSatisfy` notElem CannotTakeDamage)
    run $ DealDamage GameSource (toTarget chief) 2
    getDamage (toTarget chief) `shouldReturn` 2

  it "recognizes the pack's Cave Torch as ready, then prevents its exhausted payment" . gameTest $ \_ -> do
    hero <- getHeroWithCode "01012"
    run . (`PutObjectiveIntoPlayAttached` (toTarget hero)) =<< genEncounterCard Objectives.caveTorchDarkOfMirkwood
    let readyTorch = ReadyObjectiveWithCardCode (toCardCode Objectives.caveTorchDarkOfMirkwood)
    passesCriteria readyTorch `shouldReturn` True
    torch <- getsGame (toTarget . fromJustNote "no torch" . headMay . toList . (.entities.objectives))
    _ <- setEncounterDeck [Treacheries.drivenByShadowDarkOfMirkwood]
    run $ Exhaust torch
    passesCriteria readyTorch `shouldReturn` False

  it "offers Spider Den only while a Spider can pay its search cost" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.greatSpider]
    let spiderAvailable = CardInEncounterDeckOrDiscard (CardWithTrait Spider)
    passesCriteria spiderAvailable `shouldReturn` True
    _ <- setEncounterDeck []
    passesCriteria spiderAvailable `shouldReturn` False

  it "places a discarded location into play active as Crumbling Stairs is explored" . gameTest $ \_ -> do
    stage Locations.crumblingStairsDarkOfMirkwood
    stairs <- locationOf Locations.crumblingStairsDarkOfMirkwood
    run $ PlaceLocation stairs ActiveLocation
    _ <- setEncounterDeck [Locations.goblinDungeon]
    run $ PlaceTokens GameSource (toTarget stairs) Progress 3
    dungeon <- locationOf Locations.goblinDungeon
    (.placement) <$> getLocation dungeon `shouldReturn` ActiveLocation
    getEncounterDiscard >>= (\cards -> map toCardCode cards `shouldSatisfy` notElem (toCardCode Locations.goblinDungeon))

  it "places Goblin Trail's bonus progress directly on the current quest" . gameTest $ \pid -> do
    quest <- putQuestStage Quests.theDarkOfNibinDum
    stage Locations.goblinTrail
    trail <- locationOf Locations.goblinTrail
    run $ PlaceTokens GameSource (toTarget trail) Progress 6
    chooseAbilityOf pid trail
    (.progress) <$> getActiveQuest `shouldReturn` 6
    getDamage quest `shouldReturn` 0

  it "cannot pay Fervor of Lore's three-card cost using an encounter card in hand" . gameTest $ \pid -> do
    _ <- putCardIntoHand pid Events.sneakAttack
    _ <- putCardIntoHand pid Events.sneakAttack
    burden <- genEncounterCard Enemies.stalkingGoblin
    g <- getGame
    putGame $ g & playersL . ix pid . handL %~ (EncounterCard burden :)
    canPay (DiscardFromHandCost pid 3) `shouldReturn` False
    _ <- putCardIntoHand pid Events.sneakAttack
    canPay (DiscardFromHandCost pid 3) `shouldReturn` True

  it "does not count the Adaptation burden Guilty Conscience as a Condition attachment" . gameTest $ \_ -> do
    hero <- getHeroWithCode "01012"
    [guilty, web] <- setEncounterDeck [Treacheries.guiltyConscience, Treacheries.caughtInAWebDarkOfMirkwood]
    run $ AttachFromEncounterDiscard guilty (toTarget hero)
    getConditionAttachments `shouldReturn` []
    run $ AttachFromEncounterDiscard web (toTarget hero)
    webTarget <- getsGame (toTarget . fromJustNote "missing web" . find ((== toCardCode Treacheries.caughtInAWebDarkOfMirkwood) . toCardCode) . toList . (.entities.treacheries))
    getConditionAttachments `shouldReturn` [webTarget]
