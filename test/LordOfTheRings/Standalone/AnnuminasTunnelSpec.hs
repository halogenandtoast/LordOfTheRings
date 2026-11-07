module LordOfTheRings.Standalone.AnnuminasTunnelSpec (spec) where

import Data.Set qualified as Set
import Data.UUID qualified as UUID
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (RefreshPhase, TravelPhase))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioVictoryDisplay))
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

setReplica :: Int -> [EpicBoard] -> GameT ()
setReplica ordinal boards = do
  let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal ordinal) shared [] boards [] mempty})

spec :: Spec
spec = describe "Annúminas Siege Tunnel" do
  for_ [StagingArea, ActiveLocation] $ \placement ->
    it ("discards through the first enemy and stages its physical card without revealing it: " <> show placement) . gameTest $ \pid -> do
      run $ DoStep 2 Setup
      tunnel <- stageLocation Locations.siegeTunnel
      run $ PlaceLocation tunnel placement
      cards <- setEncounterDeck [Locations.oldForestRoad, Enemies.greenwayFootpad, Enemies.lawlessRuffian]
      run $ CheckWindows [mkWhen RoundEnds]
      getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [(.id) $ fromJustNote "missing second encounter card" $ atMay cards 1]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.lawlessRuffian]
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.oldForestRoad]
      (.threat) <$> getPlayer pid `shouldReturn` 27
      getQuestion pid `shouldReturn` Nothing

  it "discards the remaining deck if no enemy is present and does not reshuffle the discard at round end" . gameTest $ \pid -> do
    tunnel <- stageLocation Locations.siegeTunnel
    current <- getGame
    putGame $ current & phaseL .~ RefreshPhase
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.greenwayFootpad
    run $ UseCardAbility pid (LocationSource tunnel) 1 []
    getEncounterDeck `shouldReturn` []
    length <$> getEncounterDiscard `shouldReturn` 3
    getsGame (length . (.entities.enemies)) `shouldReturn` 0

  it "resolves harmlessly with an empty encounter deck" . gameTest $ \pid -> do
    tunnel <- stageLocation Locations.siegeTunnel
    _ <- setEncounterDeck []
    run $ UseCardAbility pid (LocationSource tunnel) 1 []
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getQuestion pid `shouldReturn` Nothing

  it "requires and reveals one encounter per player to travel" . twoPlayerTest testDeck $ \_ _ -> do
    tunnel <- stageLocation Locations.siegeTunnel
    current <- getGame
    putGame $ current & phaseL .~ TravelPhase
    _ <- setEncounterDeck [Locations.oldForestRoad]
    getModifiers tunnel >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    run $ TravelTo tunnel
    (.placement) <$> getLocation tunnel `shouldReturn` ActiveLocation
    getsGame (length . (.entities.locations)) `shouldReturn` 3

  it "is immune to player card effects and its blanked travel text requires no encounters" . gameTest $ \pid -> do
    tunnel <- stageLocation Locations.siegeTunnel
    hero <- heroOf pid "01012"
    run $ PlaceTokens (case hero of HeroTarget hid -> HeroSource hid; _ -> error "expected hero") (LocationTarget tunnel) Progress 1
    (.progress) <$> getLocation tunnel `shouldReturn` 0
    run $ CreateEffect $ Effect GameSource (LocationTarget tunnel) [Blank] UntilEndOfRound
    getModifiers tunnel >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo tunnel
    (.placement) <$> getLocation tunnel `shouldReturn` ActiveLocation

  it "stops its forced effect after exploration and contributes five victory points" . gameTest $ \_ -> do
    setReplica 0 [EpicBoard (GroupOrdinal 1) "Other" [] False]
    tunnel <- stageLocation Locations.siegeTunnel
    run $ PlaceLocation tunnel ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget tunnel) Progress 5
    getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.siegeTunnel]
    getsGame ((.victoryPoints) . toAttrs . (.scenario)) `shouldReturn` 5
    _ <- setEncounterDeck [Enemies.greenwayFootpad]
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    replica.epicPendingCommands `shouldBe` []

  it "resolves locally once and queues one durable command for each other team" . gameTest $ \pid -> do
    let own = EpicBoard (GroupOrdinal 0) "Own" [] False
        other = EpicBoard (GroupOrdinal 1) "Other" [] False
        lastTeam = EpicBoard (GroupOrdinal 2) "Last" [] False
    setReplica 0 [own, other, other, lastTeam]
    tunnel <- stageLocation Locations.siegeTunnel
    _ <- setEncounterDeck [Enemies.greenwayFootpad, Enemies.lawlessRuffian]
    run $ UseCardAbility pid (LocationSource tunnel) 1 []
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    let commands = replica.epicPendingCommands
    map (.epicCommandTo) commands `shouldBe` [GroupOrdinal 1, GroupOrdinal 2]
    map (.epicCommandFrom) commands `shouldBe` replicate 2 (GroupOrdinal 0)
    map (.epicCommandTarget) commands `shouldBe` replicate 2 ScenarioTarget
    map (.epicCommandEffect) commands `shouldBe` replicate 2 EpicDiscardUntilEnemy
    map (.epicCommandCard) commands `shouldBe` replicate 2 (toCardCode Locations.siegeTunnel)
    length (nub $ map (.epicCommandId) commands) `shouldBe` 2

  it "delivers to a team with no Tunnel and persists duplicate protection across save and reload" . gameTest $ \pid -> do
    run $ DoStep 2 Setup
    setReplica 1 []
    cards <- setEncounterDeck [Locations.oldForestRoad, Enemies.greenwayFootpad, Enemies.lawlessRuffian]
    let sent = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Locations.siegeTunnel) EpicDiscardUntilEnemy ScenarioTarget
        received = either error id $ eitherDecode $ encode sent
    run $ ApplyEpicCommand received
    getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [(.id) $ fromJustNote "missing second encounter card" $ atMay cards 1]
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    run $ ApplyEpicCommand received
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    length <$> getEncounterDeck `shouldReturn` 1
    (.threat) <$> getPlayer pid `shouldReturn` 27
    getQuestion pid `shouldReturn` Nothing
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    Set.member UUID.nil replica.epicAppliedCommands `shouldBe` True

  it "ignores a command addressed to another team" . gameTest $ \_ -> do
    setReplica 1 []
    _ <- setEncounterDeck [Enemies.greenwayFootpad]
    let command = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 2) (toCardCode Locations.siegeTunnel) EpicDiscardUntilEnemy ScenarioTarget
    run $ ApplyEpicCommand command
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    length <$> getEncounterDeck `shouldReturn` 1
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    replica.epicAppliedCommands `shouldBe` mempty
