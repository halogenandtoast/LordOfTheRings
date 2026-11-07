module LordOfTheRings.Standalone.AnnuminasCitySpec (spec, stageCity, setReplica) where

import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Matcher (getObjective)
import LordOfTheRings.Modifier (ModifierType (ConsideredOutOfPlay))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic))
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import TestImport

stageCity :: Int -> GameT ObjectiveId
stageCity strength = do
  run . AddToStaging =<< genEncounterCard Objectives.annuminas
  oid <- getsGame $ toId . fromJustNote "city missing" . headMay . toList . (.entities.objectives)
  run $ DoStep 1 $ PlaceTokens GameSource (ObjectiveTarget oid) Resource strength
  pure oid

setReplica :: [EpicBoard] -> GameT ()
setReplica boards = do
  let shared = fromJustNote "invalid event size" $ newAnnuminasEvent 3
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 0) shared [] boards [] mempty})

publicStrength :: GameT (Maybe Value)
publicStrength = getsGame $ \g -> case publicGame g of
  Object fields -> KeyMap.lookup "cityStrength" fields
  _ -> Nothing

spec :: Spec
spec = describe "Annúminas city objective" do
  it "subtracts damage from strength, stores no damage tokens, and repairs above its initial strength" . gameTest $ \_ -> do
    oid <- stageCity 20
    run $ DealDamage GameSource (ObjectiveTarget oid) 3
    cityStrength `shouldReturn` 17
    publicStrength `shouldReturn` Just (toJSON (17 :: Int))
    getDamage (ObjectiveTarget oid) `shouldReturn` 0
    run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 10
    cityStrength `shouldReturn` 27
    getsGame (.outcome) `shouldReturn` Nothing

  it "loses immediately when excess damage reduces strength to zero" . gameTest $ \_ -> do
    oid <- stageCity 20
    run $ DealDamage GameSource (ObjectiveTarget oid) 30
    cityStrength `shouldReturn` 0
    getsGame (.outcome) `shouldReturn` Just Lost

  it "counts both staged and engaged enemies for the end-of-round damage" . gameTest $ \pid -> do
    oid <- stageCity 20
    _ <- stageEnemy Enemies.lawlessRuffian
    eid <- stageEnemy Enemies.lawlessRuffian
    run $ EngageEnemy pid eid
    run $ UseCardAbility pid (ObjectiveSource oid) 1 []
    cityStrength `shouldReturn` 18

  it "excludes an enemy considered out of play from local damage and remote team snapshots" . gameTest $ \pid -> do
    oid <- stageCity 20
    eid <- stageEnemy Enemies.lawlessRuffian
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [ConsideredOutOfPlay] UntilEndOfRound
    current <- getGame
    epicBoardTargets (epicBoardFor (GroupOrdinal 1) "Other" current) `shouldBe` []
    run $ UseCardAbility pid (ObjectiveSource oid) 1 []
    cityStrength `shouldReturn` 20

  it "initializes the Epic city from shared strength without contributing another fifty" . gameTest $ \_ -> do
    setReplica []
    oid <- stageCity 50
    (.resources) <$> getObjective oid `shouldReturn` 50
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    replica.epicPendingDeltas `shouldBe` []

  it "mirrors its team's damage and repairs in both the city card and shared outbox" . gameTest $ \_ -> do
    setReplica []
    oid <- stageCity 50
    run $ DealDamage GameSource (ObjectiveTarget oid) 9
    cityStrength `shouldReturn` 41
    (.resources) <$> getObjective oid `shouldReturn` 41
    run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 10
    (.resources) <$> getObjective oid `shouldReturn` 51
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.deltaAmount) replica.epicPendingDeltas `shouldBe` [-9, 10]

  it "allows a team without the city card to damage and repair the same shared city" . gameTest $ \_ -> do
    setReplica []
    runQueueT $ damageAnnuminas GameSource 9
    void mainLoop
    cityStrength `shouldReturn` 41
    runQueueT $ repairAnnuminas GameSource 10
    void mainLoop
    cityStrength `shouldReturn` 51
    publicStrength `shouldReturn` Just (toJSON (51 :: Int))

  it "does not expose a city tracker in another scenario" . gameTest $ \_ -> do
    publicStrength `shouldReturn` Just Null

  it "mirrors remote updates without emitting another contribution" . gameTest $ \_ -> do
    setReplica []
    oid <- stageCity 50
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    ident <- getRandom
    let (shared, _) = applySharedDelta (SharedDelta ident (GroupOrdinal 1) AnnuminasCityStrength (-9)) replica.epicShared
    run $ SyncEpic shared
    run $ SyncEpic shared
    (.resources) <$> getObjective oid `shouldReturn` 41
    Just updated <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    updated.epicPendingDeltas `shouldBe` []

  it "counts enemies at other teams once and excludes its own stale public snapshot" . gameTest $ \pid -> do
    card <- genEncounterCard Enemies.lawlessRuffian
    eid <- getRandom
    let enemyTarget = EpicTarget (EnemyTarget eid) card EpicEnemy False 0 False False True
        own = EpicBoard (GroupOrdinal 0) "Own" [enemyTarget] False Nothing
        remote = EpicBoard (GroupOrdinal 1) "Other" [enemyTarget, enemyTarget] False Nothing
    setReplica [own, remote]
    oid <- stageCity 50
    _ <- stageEnemy Enemies.lawlessRuffian
    run $ UseCardAbility pid (ObjectiveSource oid) 1 []
    cityStrength `shouldReturn` 47
