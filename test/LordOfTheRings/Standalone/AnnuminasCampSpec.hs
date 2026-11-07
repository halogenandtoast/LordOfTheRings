module LordOfTheRings.Standalone.AnnuminasCampSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (toCard)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Public (hiddenCards)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioVictoryDisplay))
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import TestImport

stageThree :: GameT QuestId
stageThree = do
  run . PutQuestIntoPlay =<< genEncounterCard Quests.apprehendingTheKiller
  toId <$> getActiveQuest

capture :: QuestId -> PlayerId -> CardCode -> GameT CardId
capture qid pid code = do
  hero <- getHeroWithCode code
  hero.owner `shouldBe` pid
  let cid = (toCard hero).id
  run $ CaptureHeroUnderQuest qid hero.id
  pure cid

spec :: Spec
spec = describe "Annúminas Enemy Camp" do
  it "captures a hero out of play and keeps its identity and owner facedown under the quest" . gameTest $ \pid -> do
    stage <- stageThree
    cid <- capture stage pid "01012"
    getsGame (length . (.entities.heroes)) `shouldReturn` 2
    current <- getGame
    let cards = concatMap (questUnderneath . toAttrs) current.entities.quests
    map (.id) cards `shouldBe` [cid]
    [c.owner | PlayerCard c <- cards] `shouldBe` [Just pid]
    hiddenCards current `shouldSatisfy` member cid
    run CheckEliminations
    (.eliminated) <$> getPlayer pid `shouldReturn` False

  it "discards exactly one random captive at round end while in play" . gameTest $ \pid -> do
    stage <- stageThree
    _ <- stageLocation Locations.enemyCamp
    former <- capture stage pid "01012"
    latter <- capture stage pid "01007"
    run $ CheckWindows [mkWhen RoundEnds]
    current <- getGame
    let remaining = concatMap (questUnderneath . toAttrs) current.entities.quests
    length remaining `shouldBe` 1
    map (.id) remaining `shouldSatisfy` all (`elem` [former, latter])
    discarded <- (.discard) <$> getPlayer pid
    length (filter ((`elem` [former, latter]) . (.id)) discarded) `shouldBe` 1

  it "returns every captive to its owner from the victory display, including after save and reload" . twoPlayerTest testDeck $ \pid other -> do
    stage <- stageThree
    camp <- stageLocation Locations.enemyCamp
    -- Each player has the same deck; select captives by owner.
    otherHero <- fromJustNote "missing other hero" . find ((== other) . (.owner)) . toList <$> getsGame (.entities.heroes)
    own <- fromJustNote "missing own hero" . find ((== pid) . (.owner)) . toList <$> getsGame (.entities.heroes)
    run $ CaptureHeroUnderQuest stage own.id
    run $ CaptureHeroUnderQuest stage otherHero.id
    run $ PlaceLocation camp ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget camp) Progress 5
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (concatMap (questUnderneath . toAttrs) . (.entities.quests)) `shouldReturn` []
    heroes <- toList <$> getsGame (.entities.heroes)
    [h.owner | h <- heroes, (toCard h).id == (toCard own).id] `shouldBe` [pid]
    [h.owner | h <- heroes, (toCard h).id == (toCard otherHero).id] `shouldBe` [other]
    getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.enemyCamp]
    getsGame ((.victoryPoints) . toAttrs . (.scenario)) `shouldReturn` 5

  it "sends the appropriate captive effect to every other team once" . gameTest $ \_ -> do
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
        other = EpicBoard (GroupOrdinal 1) "Other" [] False Nothing
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 0) shared [] [other, other] [] mempty})
    camp <- stageLocation Locations.enemyCamp
    run $ CheckWindows [mkWhen RoundEnds]
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.epicCommandEffect) replica.epicPendingCommands `shouldBe` [EpicDiscardCaptive]
    run $ PlaceLocation camp ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget camp) Progress 5
    run $ CheckWindows [mkWhen RoundEnds]
    Just updated <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.epicCommandEffect) updated.epicPendingCommands `shouldBe` [EpicDiscardCaptive, EpicRescueCaptives]

  it "does not eliminate a player whose only hero is captive, but does when that captive is discarded" . gameTestWith (deckList ["01012"] testDeck.cards) $ \pid -> do
    stage <- stageThree
    _ <- stageLocation Locations.enemyCamp
    _ <- capture stage pid "01012"
    run CheckEliminations
    (.eliminated) <$> getPlayer pid `shouldReturn` False
    run $ CheckWindows [mkWhen RoundEnds]
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "leaves captives under other numbered stages alone" . gameTest $ \pid -> do
    run . PutQuestIntoPlay =<< genEncounterCard Quests.runningOutOfTime
    stage <- toId <$> getActiveQuest
    _ <- capture stage pid "01012"
    _ <- stageLocation Locations.enemyCamp
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (length . concatMap (questUnderneath . toAttrs) . (.entities.quests)) `shouldReturn` 1

  it "discards attachments and resets all hero tokens before a captive returns" . gameTest $ \pid -> do
    stage <- stageThree
    hero <- getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.hornOfGondor (HeroTarget hero.id)
    run $ PlaceTokens GameSource (HeroTarget hero.id) Damage 2
    run $ PlaceTokens GameSource (HeroTarget hero.id) Resource 4
    cid <- capture stage pid "01012"
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    run $ ResolveEpicEffect (toCardCode Locations.enemyCamp) EpicRescueCaptives ScenarioTarget
    restored <- fromJustNote "hero not rescued" . find ((== cid) . (.id) . toCard) . toList <$> getsGame (.entities.heroes)
    restored.resources `shouldBe` 0
    restored.damage `shouldBe` 0
    discarded <- (.discard) <$> getPlayer pid
    map toCardCode discarded `shouldSatisfy` elem (toCardCode Attachments.hornOfGondor)

  it "delivers captive rescue to a team without the Camp and persists duplicate protection" . gameTest $ \pid -> do
    stage <- stageThree
    cid <- capture stage pid "01012"
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
        command = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Locations.enemyCamp) EpicRescueCaptives ScenarioTarget
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 1) shared [] [] [] mempty})
    run $ ApplyEpicCommand command
    saved <- getGame
    restored <- either error pure $ eitherDecode $ encode saved
    putGame =<< preloadModifiers restored
    run $ ApplyEpicCommand command
    getsGame (length . filter ((== cid) . (.id) . toCard) . toList . (.entities.heroes)) `shouldReturn` 1
    getsGame (concatMap (questUnderneath . toAttrs) . (.entities.quests)) `shouldReturn` []

  it "requires a full encounter reveal per player to travel and is immune to player-card progress" . twoPlayerTest testDeck $ \pid _ -> do
    camp <- stageLocation Locations.enemyCamp
    hero <- heroOf pid "01012"
    run $ PlaceTokens (case hero of HeroTarget hid -> HeroSource hid; _ -> GameSource) (LocationTarget camp) Progress 1
    (.progress) <$> getLocation camp `shouldReturn` 0
    current <- getGame
    putGame $ current & phaseL .~ TravelPhase
    _ <- setEncounterDeck [Locations.oldForestRoad]
    getModifiers camp >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    run $ TravelTo camp
    (.placement) <$> getLocation camp `shouldReturn` ActiveLocation
    getsGame (length . (.entities.locations)) `shouldReturn` 3

  it "does not activate a departed Camp outside the victory display" . gameTest $ \pid -> do
    stage <- stageThree
    _ <- capture stage pid "01012"
    camp <- stageLocation Locations.enemyCamp
    run $ RemoveFromGame $ LocationTarget camp
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (length . concatMap (questUnderneath . toAttrs) . (.entities.quests)) `shouldReturn` 1
    getsGame (length . (.entities.heroes)) `shouldReturn` 2

  it "cannot return a captive to an eliminated player" . twoPlayerTest testDeck $ \pid _ -> do
    stage <- stageThree
    hero <- fromJustNote "missing own hero" . find ((== pid) . (.owner)) . toList <$> getsGame (.entities.heroes)
    let cid = (toCard hero).id
    run $ CaptureHeroUnderQuest stage hero.id
    run $ EliminatePlayer pid
    run $ ResolveEpicEffect (toCardCode Locations.enemyCamp) EpicRescueCaptives ScenarioTarget
    getsGame (length . filter ((== cid) . (.id) . toCard) . toList . (.entities.heroes)) `shouldReturn` 0
