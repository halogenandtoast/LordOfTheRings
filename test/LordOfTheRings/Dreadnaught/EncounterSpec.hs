module LordOfTheRings.Dreadnaught.EncounterSpec (spec) where

import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationTokens))
import LordOfTheRings.Matcher (LocationMatcher (LocationWithCardCode), getEnemy, select)
import LordOfTheRings.Modifier (ModifierType (CannotAttack))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery (Treachery, newTreachery)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
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

isle :: GameT LocationId
isle = do
  run . AddToStaging =<< genEncounterCard Locations.isleOfFortune
  getsGame
    ( toId
        . fromJustNote "isle missing"
        . find ((== toCardCode Locations.isleOfFortune) . toCardCode)
        . toList
        . (.entities.locations)
    )

pureIt :: String -> IO () -> Spec
pureIt = it

spec :: Spec
spec = describe "Dreadnaught encounters" do
  it "Isle of Fortune enters with one resource per player without being revealed"
    . twoPlayerTest (coreDeck "tactics")
    $ \_ _ -> do
      lid <- isle
      countTokens Resource . locationTokens . toAttrs <$> getLocation lid `shouldReturn` 2
      _ <- staged Enemies.corsairSkiffDreadnaught
      countTokens Resource . locationTokens . toAttrs <$> getLocation lid `shouldReturn` 1
      _ <- staged Enemies.ophidian
      countTokens Resource . locationTokens . toAttrs <$> getLocation lid `shouldReturn` 1

  it "moves Isle of Fortune's resources to heroes when explored as the active location" . gameTest $ \pid -> do
    lid <- isle
    hero <- getHeroWithCode "01012"
    run $ TravelTo lid
    run $ PlaceTokens GameSource (toTarget lid) Progress 5
    chooseAbilityOf pid lid
    chooseTarget pid hero
    getHeroResources (toId hero) `shouldReturn` 1

  it "Out of the Mist gains surge when its chosen enemy cannot attack" . gameTest $ \pid -> do
    eid <- staged Enemies.corsairPlunderer
    run $ CreateEffect $ Effect GameSource (toTarget eid) [CannotAttack] UntilEndOfRound
    _ <- setEncounterDeck [Treacheries.outOfTheMist, Locations.forestGate]
    run RevealEncounterCard
    resolveRevealed
    (.placement) <$> getEnemy eid `shouldReturn` EngagedWith pid
    length <$> select (LocationWithCardCode $ toCardCode Locations.forestGate) `shouldReturn` 1
    getEncounterDeck `shouldReturn` []

  it "Out of the Mist gains surge if the resulting attack is cancelled" . gameTest $ \pid -> do
    eid <- staged Enemies.cavernWarg
    hero <- toTarget <$> getHeroWithCode "01012"
    torch <- genEncounterCard Objectives.caveTorchDarkOfMirkwood
    run $ PutObjectiveIntoPlayAttached torch hero
    _ <- setEncounterDeck [Treacheries.outOfTheMist, Locations.forestGate, Locations.forestGate, Locations.gladdenFields]
    run RevealEncounterCard
    resolveRevealed
    chooseAbilityOf pid eid
    passActions pid
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
    getQuestion pid `shouldReturn` Nothing
    map toCardCode <$> getEncounterDeck `shouldReturn` []
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.gladdenFields]
    length <$> select (LocationWithCardCode $ toCardCode Locations.gladdenFields) `shouldReturn` 1
    getDamage hero `shouldReturn` 0

  pureIt "loads saved games and treacheries written before Epic and attack-tracking state" do
    let oldScenario = case toJSON passageThroughMirkwood of
          Object fields -> Object $ KeyMap.delete "epic" fields
          value -> value
    fmap toJSON (fromJSON oldScenario :: Result Scenario)
      `shouldBe` Success (toJSON passageThroughMirkwood)
    card <- genEncounterCard Treacheries.outOfTheMist
    treachery <- newTreachery card
    let oldTreachery = case toJSON treachery of
          Object fields -> Object $ KeyMap.delete "meta" fields
          value -> value
    fmap toJSON (fromJSON oldTreachery :: Result Treachery) `shouldBe` Success (toJSON treachery)
