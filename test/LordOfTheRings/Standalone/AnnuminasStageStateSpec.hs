module LordOfTheRings.Standalone.AnnuminasStageStateSpec (spec) where

import Data.List qualified as List
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic))
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import TestImport

stageState :: GameT EpicStage
stageState = getsGame $ fromJustNote "missing public stage state" . epicBoardStage . epicBoardFor (GroupOrdinal 0) "City"

spec :: Spec
spec = describe "Annúminas public Epic stage state" do
  it "uses modified staging threat, including a location's X and global threat adjustments" . gameTest $ \_ -> do
    _ <- stageLocation Locations.gateOfAnnuminas
    _ <- stageEnemy Enemies.lawlessRuffian
    run $ CreateEffect $ Effect GameSource GameTarget [StagingThreatBonus 3] UntilEndOfRound
    epicStageThreat <$> stageState `shouldReturn` 7
    run $ CreateEffect $ Effect GameSource GameTarget [StagingThreatBonus (-100)] UntilEndOfRound
    epicStageThreat <$> stageState `shouldReturn` 0

  it "publishes the main quest's stage, progress and modified requirement while excluding side quests" . gameTest $ \pid -> do
    stage <- putQuestStage Quests.fliesAndSpiders
    run $ Flip stage
    run $ CreateEffect $ Effect GameSource stage [QuestPointModifier 3] UntilEndOfRound
    run $ PlaceTokens GameSource stage Progress 2
    side <- genPlayerCard Quests.gatherInformation (Just pid)
    run $ PutSideQuestIntoPlay pid side
    quests <- epicStageQuests <$> stageState
    quests `shouldBe` [EpicQuest (toCardCode Quests.fliesAndSpiders) 1 2 (Just 11) True]

  it "publishes all physical Host victories without publishing hands, deck order or facedown cards beneath quests" . gameTest $ \pid -> do
    stage <- putQuestStage Quests.fliesAndSpiders
    handCard <- putCardIntoHand pid Events.aTestOfWill
    prisoner <- PlayerCard <$> genPlayerCard Events.feint (Just pid)
    run $ PlaceUnderneath stage [prisoner]
    deck <- setEncounterDeck [Enemies.greenwayFootpad, Enemies.lawlessRuffian]
    replicateM_ 2 do
      eid <- stageEnemy Enemies.hostOfAngmar
      run $ AddToVictoryDisplay (EnemyTarget eid)
    current <- getGame
    let board = epicBoardFor (GroupOrdinal 0) "City" current
        publicStage = fromJustNote "missing stage" board.epicBoardStage
    publicStage.epicStageVictoryCards `shouldBe` replicate 2 (toCardCode Enemies.hostOfAngmar)
    for_ (handCard.id : prisoner.id : map (.id) deck) $ \cid ->
      show (encode board) `shouldSatisfy` (not . List.isInfixOf (show cid))

  it "does not offer an ended team as a live stage" . gameTest $ \_ -> do
    _ <- stageLocation Locations.gateOfAnnuminas
    run $ GameOver Lost
    current <- getGame
    let board = epicBoardFor (GroupOrdinal 0) "City" current
    board.epicBoardStage `shouldBe` Nothing
    board.epicBoardTargets `shouldBe` []

  it "exposes public boards while keeping command delivery and retry records private" . gameTest $ \_ -> do
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
        board = EpicBoard (GroupOrdinal 1) "Gate" [] False (Just $ EpicStage 7 [] [] False 0 False [])
        command = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Locations.siegeTunnel) EpicDiscardUntilEnemy ScenarioTarget
        contribution = SharedDelta UUID.nil (GroupOrdinal 0) AnnuminasCityStrength (-1)
        replica = EpicReplica (GroupOrdinal 0) shared [contribution] [board] [command] (Set.singleton UUID.nil) (Set.singleton UUID.nil) Nothing
    current <- getGame
    let updated = current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just replica})
        fields = parseMaybe (withObject "game" $ \o -> o .: "scenario" >>= withObject "scenario" (\s -> s .: "epic" >>= withObject "epic" pure)) $ publicGame updated
    publicFields <- maybe (error "missing public Epic replica") pure fields
    for_ ["epicPendingDeltas", "epicPendingCommands", "epicPendingRequests", "epicAppliedCommands"] $ \key ->
      KeyMap.lookup key publicFields `shouldBe` Nothing
    KeyMap.lookup "epicBoards" publicFields `shouldBe` Just (toJSON [board])
    KeyMap.lookup "epicShared" publicFields `shouldBe` Just (toJSON shared)

  it "reads preexisting Epic boards that have no public stage field" do
    let legacy = object
          [ "epicBoardGroup" .= GroupOrdinal 1
          , "epicBoardName" .= ("Fleet" :: Text)
          , "epicBoardTargets" .= ([] :: [EpicTarget])
          , "epicBoardHasWarship" .= False
          ]
    eitherDecode (encode legacy) `shouldBe` Right (EpicBoard (GroupOrdinal 1) "Fleet" [] False Nothing)
    pure () :: IO ()

  it "preserves public stage values through command-replica serialization" do
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
        stage = EpicStage 7 [EpicQuest "01120" 2 40 (Just 40) True] [toCardCode Enemies.hostOfAngmar] False 0 False []
        board = EpicBoard (GroupOrdinal 1) "Gate" [] False (Just stage)
        replica = EpicReplica (GroupOrdinal 0) shared [] [board] [] mempty mempty Nothing
    eitherDecode (encode replica) `shouldBe` Right replica
    pure () :: IO ()

  it "reads stage snapshots saved before healing eligibility was published" do
    let legacy = object
          [ "epicStageThreat" .= (3 :: Int)
          , "epicStageQuests" .= ([] :: [EpicQuest])
          , "epicStageVictoryCards" .= ([] :: [CardCode])
          ]
    eitherDecode (encode legacy) `shouldBe` Right (EpicStage 3 [] [] False 0 False [])
    pure () :: IO ()

  it "publishes healing eligibility using the actual character healing limits" . gameTest $ \pid -> do
    epicStageHealingAvailable <$> stageState `shouldReturn` False
    hero <- heroOf pid "01012"
    run $ PlaceTokens GameSource hero Damage 1
    epicStageHealingAvailable <$> stageState `shouldReturn` True
    run $ CreateEffect $ Effect GameSource hero [HealLimit 0] UntilEndOfRound
    epicStageHealingAvailable <$> stageState `shouldReturn` False
    run $ EndEffects UntilEndOfRound
    epicStageHealingAvailable <$> stageState `shouldReturn` True

  it "publishes only the count of set-aside Hosts and updates it when one enters play" . gameTest $ \_ -> do
    replicateM_ 3 $ run . SetAside =<< genEncounterCard Enemies.hostOfAngmar
    epicStageSetAsideHosts <$> stageState `shouldReturn` 3
    run $ RequestAnnuminasHost (toCardCode Quests.manTheWallsAnnuminas) (GroupOrdinal 0)
    epicStageSetAsideHosts <$> stageState `shouldReturn` 2
    getsGame (length . (.entities.enemies)) `shouldReturn` 1

  it "publishes drawing eligibility without exposing a player's deck" . gameTest $ \pid -> do
    epicStageDrawingAvailable <$> stageState `shouldReturn` True
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCardsFromCardEffects] UntilEndOfRound
    epicStageDrawingAvailable <$> stageState `shouldReturn` False
    run $ EndEffects UntilEndOfRound
    epicStageDrawingAvailable <$> stageState `shouldReturn` True
