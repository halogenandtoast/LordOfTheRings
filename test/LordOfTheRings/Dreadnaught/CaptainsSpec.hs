module LordOfTheRings.Dreadnaught.CaptainsSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (..), PlayerMatcher (PlayerWithId), getEnemy, select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Scenario.Types (corsairDeckL)
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "enemy missing"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.enemies)
    )

ship :: PlayerId -> CardDef -> GameT Target
ship pid def = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
  getsGame
    ( toTarget
        . fromJustNote "ship missing"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.allies)
    )

spec :: Spec
spec = describe "Dreadnaught captains" do
  it "Captain of the Haven eliminates its engaged player only when their deck is empty" . gameTest $ \pid -> do
    eid <- staged Enemies.captainOfTheHaven
    run $ EngageEnemy pid eid
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    (.eliminated) <$> getPlayer pid `shouldReturn` False
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ []
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "Ophidian damages a defender before shadow and combat damage, and limits attackers" . gameTest $ \_ -> do
    eid <- staged Enemies.ophidian
    defender <- toTarget <$> getHeroWithCode "01012"
    run $ CheckWindows [mkAfter $ DeclaredAsDefender defender eid]
    getDamage defender `shouldReturn` 2
    getModifiers eid >>= (`shouldSatisfy` elem (AttackersPerRoundLimit 1))

  it "Descendant of Castamir engages two Corsairs with one shadow card each" . gameTest $ \pid -> do
    cards <-
      traverse
        genEncounterCard
        [Enemies.corsairPlunderer, Enemies.fierceMarauder, Enemies.southronSailorDreadnaught]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (corsairDeckL ?~ cards)
    _ <- setEncounterDeck [Locations.forestGate, Locations.forestGate]
    eid <- staged Enemies.descendantOfCastamir
    run $ EngageEnemy pid eid
    engaged <- select $ EnemyEngagedWith $ PlayerWithId pid
    length engaged `shouldBe` 3
    boarders <- traverse getEnemy $ filter (/= eid) engaged
    map (length . (.shadowCards)) boarders `shouldBe` [1, 1]

  it "Hydraken prepares three shadow cards and raises threat before its additional attack" . gameTest $ \pid -> do
    eid <- staged Enemies.hydraken
    run $ EngageEnemy pid eid
    _ <- setEncounterDeck $ replicate 3 Locations.forestGate
    run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    (.threat) <$> getPlayer pid `shouldReturn` 4
    length . (.shadowCards) <$> getEnemy eid `shouldReturn` 3
    passActions pid
    chooseTarget pid =<< getHeroWithCode "01012"
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    (.threat) <$> getPlayer pid `shouldReturn` 4

  it "Varjax allows each excess point to be assigned to a different surviving ship" . gameTest $ \pid -> do
    eid <- staged Enemies.varjax
    firstShip <- ship pid Objectives.corsairSkirmisherShip
    secondShip <- ship pid Objectives.corsairSkirmisherShipDreadnaught
    defender <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource firstShip Damage 5
    run $ PlaceTokens GameSource defender Damage 3
    messages <- attackDamage pid eid defender 3
    pushAll messages
    void mainLoop
    chooseTarget pid firstShip
    getDamage secondShip `shouldReturn` 1

  it "Commander of the Dreadnaught makes the boss attack the same player" . gameTest $ \pid -> do
    _ <- staged Enemies.dreadnaught
    commander <- staged Enemies.commanderOfTheDreadnaught
    run $ EngageEnemy pid commander
    defender <- ship pid Objectives.corsairSkirmisherShip
    _ <- setEncounterDeck [Locations.forestGate]
    run $ CheckWindows [mkAfter $ EnemyAttacking commander pid]
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 3
