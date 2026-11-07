module LordOfTheRings.Dreadnaught.EpicEffectsSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Criteria (Criterion (EpicTargetsExist))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Window (passesCriteria)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (AnyEnemy), getAlly, select)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioSetAside))
import TestImport

setReplica :: GroupOrdinal -> [EpicBoard] -> GameT ()
setReplica ordinal boards = do
  let shared = fromJustNote "shared state" $ newSharedEvent 4
  g <- getGame
  putGame
    $ g
    & scenarioL
    %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica ordinal shared [] boards [] mempty mempty Nothing})

spec :: Spec
spec = describe "Dreadnaught Epic effects" do
  it "offers explicit remote targets without exposing them to ordinary enemy matchers" . gameTest $ \pid -> do
    card <- genEncounterCard Enemies.fierceMarauder
    eid <- getRandom
    let target = EpicTarget (EnemyTarget eid) card EpicEnemy False 0 False False True
        board = EpicBoard (GroupOrdinal 1) "The second group" [target] False Nothing
    setReplica (GroupOrdinal 0) [board]
    select AnyEnemy `shouldReturn` []
    run $ ChooseEpicTarget pid (toCardCode Objectives.goldenWingUpgraded) EpicEnemy (EpicDamage 6)
    chooseOptionMatching "choose remote enemy" pid (const True)
    Just replica <- getsGame (scenarioEpic . toAttrs . (.scenario))
    length replica.epicPendingCommands `shouldBe` 1
    map (.epicCommandTo) replica.epicPendingCommands `shouldBe` [GroupOrdinal 1]
    map (.epicCommandTarget) replica.epicPendingCommands `shouldBe` [EnemyTarget eid]
    select AnyEnemy `shouldReturn` []

  it "delivers a remote effect once, even if the coordinator retries it" . gameTest $ \_ -> do
    setReplica (GroupOrdinal 1) []
    run . AddToStaging =<< genEncounterCard Enemies.fierceMarauder
    eid <- getsGame (toId . fromJustNote "enemy missing" . headMay . toList . (.entities.enemies))
    let command =
          EpicCommand
            UUID.nil
            (GroupOrdinal 0)
            (GroupOrdinal 1)
            (toCardCode Objectives.goldenWingUpgraded)
            (EpicDamage 1)
            (EnemyTarget eid)
    run $ ApplyEpicCommand command
    run $ ApplyEpicCommand command
    getDamage (toTarget eid) `shouldReturn` 1

  it "Belfalas Shipyard's effect heals, readies and upgrades without flipping an upgraded ship back"
    . gameTest
    $ \pid -> do
      run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.goldenWing
      chooseTarget pid =<< getHeroWithCode "01012"
      aid <- getsGame (toId . fromJustNote "ship missing" . headMay . toList . (.entities.allies))
      run $ PlaceTokens GameSource (toTarget aid) Damage 8
      run $ Exhaust (toTarget aid)
      run $ ResolveEpicEffect (toCardCode Locations.belfalasShipyard) EpicUpgradeShip (toTarget aid)
      chooseLabel "$label.pass" pid
      toCardCode <$> getAlly aid `shouldReturn` toCardCode Objectives.goldenWingUpgraded
      getDamage (toTarget aid) `shouldReturn` 3
      (\a -> (toAttrs a).allyExhausted) <$> getAlly aid `shouldReturn` False
      run $ ResolveEpicEffect (toCardCode Locations.belfalasShipyard) EpicUpgradeShip (toTarget aid)
      toCardCode <$> getAlly aid `shouldReturn` toCardCode Objectives.goldenWingUpgraded
      getDamage (toTarget aid) `shouldReturn` 0

  it "Gondorian Warship is assigned when revealed and returns to set-aside when destroyed" . gameTest $ \pid -> do
    _ <- setEncounterDeck [Objectives.gondorianWarship]
    run RevealEncounterCard
    chooseTarget pid pid
    aid <- getsGame (toId . fromJustNote "ship missing" . headMay . toList . (.entities.allies))
    run $ Destroy GameSource (toTarget aid)
    cards <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    map toCardCode cards `shouldSatisfy` elem (toCardCode Objectives.gondorianWarship)
    getEncounterDiscard `shouldReturn` []

  it "requires a useful local or remote ship before paying for a ship upgrade" . gameTest $ \_ -> do
    passesCriteria (EpicTargetsExist EpicShip) `shouldReturn` False
    card <- genEncounterCard Objectives.goldenWing
    aid <- getRandom
    let target = EpicTarget (AllyTarget aid) card EpicShip True 0 False True False
        board = EpicBoard (GroupOrdinal 1) "The second group" [target] False Nothing
    setReplica (GroupOrdinal 0) [board]
    passesCriteria (EpicTargetsExist EpicShip) `shouldReturn` True
    setReplica (GroupOrdinal 0) [board {epicBoardTargets = [target {epicTargetUpgradeable = False}]}]
    passesCriteria (EpicTargetsExist EpicShip) `shouldReturn` False

  it "keeps each group's quest tokens local while sharing its additions and removals" . gameTest $ \_ -> do
    setReplica (GroupOrdinal 0) []
    target <- putQuestStage Quests.searchingTheHighSeas
    let shared = fromJustNote "shared state" $ newSharedEvent 4
        (others, _) = applySharedDelta (SharedDelta UUID.nil (GroupOrdinal 1) SearchingTheHighSeasProgress 20) shared
    run $ SyncEpic others
    (.progress) <$> getActiveQuest `shouldReturn` 0
    run $ PlaceTokens GameSource target Progress 4
    (.progress) <$> getActiveQuest `shouldReturn` 4
    run $ RemoveTokens GameSource target Progress 2
    (.progress) <$> getActiveQuest `shouldReturn` 2
    Just replica <- getsGame (scenarioEpic . toAttrs . (.scenario))
    sharedCounter SearchingTheHighSeasProgress replica.epicShared `shouldBe` 22

  it "rebuilds an Epic target choice when another group's public board changes" . gameTest $ \pid -> do
    card <- genEncounterCard Enemies.fierceMarauder
    firstId <- getRandom
    secondId <- getRandom
    let target eid = EpicTarget (EnemyTarget eid) card EpicEnemy False 0 False False True
        board eid = EpicBoard (GroupOrdinal 1) "The second group" [target eid] False Nothing
        choose = ChooseEpicTarget pid (toCardCode Objectives.goldenWingUpgraded) EpicEnemy (EpicDamage 6)
    setReplica (GroupOrdinal 0) [board firstId]
    run choose
    Just asked <- getQuestion pid
    questionRebuild asked `shouldBe` Just choose
    setReplica (GroupOrdinal 0) [board secondId]
    void $ runDebug $ Run []
    Just refreshed <- getQuestion pid
    let offered =
          concatMap
            (\case Label _ messages -> [t | RequestEpicEffect _ _ _ t <- messages]; _ -> [])
            (questionChoices refreshed)
    offered `shouldBe` [EnemyTarget secondId]

  it "acknowledges a remote command safely when its target has already left play" . gameTest $ \_ -> do
    setReplica (GroupOrdinal 1) []
    missing <- getRandom
    let command =
          EpicCommand
            UUID.nil
            (GroupOrdinal 0)
            (GroupOrdinal 1)
            (toCardCode Objectives.goldenWingUpgraded)
            (EpicDamage 6)
            (EnemyTarget missing)
    run $ ApplyEpicCommand command
    Just replica <- getsGame (scenarioEpic . toAttrs . (.scenario))
    replica.epicAppliedCommands `shouldSatisfy` member UUID.nil
    getsGame (length . (.entities.enemies)) `shouldReturn` 0

  it "does not offer targets or set-aside Warships from a finished group" . gameTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Enemies.fierceMarauder
    run $ GameOver Lost
    g <- getGame
    let board = epicBoardFor (GroupOrdinal 1) "Finished group" g
    board.epicBoardTargets `shouldBe` []
    board.epicBoardHasWarship `shouldBe` False
