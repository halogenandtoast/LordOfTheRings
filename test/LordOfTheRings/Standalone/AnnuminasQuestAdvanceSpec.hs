module LordOfTheRings.Standalone.AnnuminasQuestAdvanceSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability.Types (isForcedAbility)
import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilSpent))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Runner (epicRoundPhaseOf)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Helpers.Window (getWindowAbilities)
import LordOfTheRings.Log.Narrator (narrationFor)
import LordOfTheRings.Log.Refs (sourceCardCode)
import LordOfTheRings.Modifier (ModifierType (CannotBeDefeated))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur)
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers qualified as Dol
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioMeta, scenarioQuestDeck, scenarioVictoryDisplay)
import LordOfTheRings.Standalone.AnnuminasCitySpec (setReplica)
import LordOfTheRings.Window (WindowType (RoundBegins), mkWhen)
import TestImport

external :: Message -> GameT ()
external = void . runDebug

setRoundPhase :: EpicRoundPhase -> GameT ()
setRoundPhase phase = do
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = fmap (\r -> r {epicRoundState = Just $ EpicRoundState current.round phase}) a.scenarioEpic})

freezeBeginning :: GameT ()
freezeBeginning = do
  abilities <- filter isForcedAbility <$> getWindowAbilities [mkWhen RoundBegins]
  external $ FreezeEpicRoundBeginning abilities

resourceTotal :: GameT Int
resourceTotal = getsGame $ sum . map (.resources) . toList . (.entities.heroes)

spec :: Spec
spec = describe "Annúminas coordinated quest advancement" do
  it "dispatches raw round resets once before revelations and holds resources behind the global beginning barrier" . gameTest $ \_ -> do
    let shared = fromJustNote "invalid event" $ newDolGuldurEvent 3
        replica = EpicReplica (GroupOrdinal 0) shared [] [] [] mempty mempty (Just $ EpicRoundState 0 EpicRoundExpired)
    current <- getGame
    putGame $ current & scenarioL .~ overAttrs (\a -> a {scenarioEpic = Just replica, scenarioMeta = toJSON $ Dol.Meta 99 []}) attackOnDolGuldur
    external BeginEpicRoundBeginning
    getsGame (.round) `shouldReturn` 1
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundBeginning
    getsGame (Dol.roundSiege . Dol.getMeta . toAttrs . (.scenario)) `shouldReturn` 0
    resourceTotal `shouldReturn` 0
    -- A revelation in the new round may record siege. Releasing its beginning
    -- window must not dispatch raw BeginRound and erase that new value again.
    afterReset <- getGame
    putGame $ afterReset & scenarioL %~ overAttrs (\a -> a {scenarioMeta = toJSON $ Dol.Meta 7 []})
    freezeBeginning
    external ReleaseEpicRoundBeginning
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundBegan
    resourceTotal `shouldReturn` 0
    getsGame (Dol.roundSiege . Dol.getMeta . toAttrs . (.scenario)) `shouldReturn` 7
    external BeginEpicRoundResources
    getsGame (.round) `shouldReturn` 1
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundIdle
    resourceTotal `shouldReturn` 3
    getsGame (Dol.roundSiege . Dol.getMeta . toAttrs . (.scenario)) `shouldReturn` 7

  it "preserves the old Man the Walls trigger through a saved prepared quest and resolves it before any resources" . gameTest $ \_ -> do
    setReplica []
    oldTarget <- putQuestStage Quests.manTheWallsAnnuminas
    run $ Flip oldTarget
    resolveRevealed
    old <- getActiveQuest
    run $ CreateEffect $ Effect GameSource oldTarget [CannotBeDefeated] UntilSpent
    distractor <- genEncounterCard Quests.runningOutOfTime
    chosen <- genEncounterCard Quests.stopTheAttack
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioQuestDeck = [distractor, chosen]})
    run $ PlaceTokens GameSource oldTarget Progress 50
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.manTheWallsAnnuminas
    blockedLog <- maybe (pure []) id $ narrationFor $ CompleteQuest $ toId old
    length blockedLog `shouldBe` 0
    run . SetAside =<< genEncounterCard Enemies.hostOfAngmar
    setRoundPhase EpicRoundExpired
    external BeginEpicRoundBeginning
    freezeBeginning
    external $ PrepareEpicQuestAdvance EpicDefeatQuest chosen
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundPrepared
    replacement <- getActiveQuest
    (toCard replacement).id `shouldBe` chosen.id
    replacement.flipped `shouldBe` False
    getsGame (map (.id) . scenarioQuestDeck . toAttrs . (.scenario)) `shouldReturn` [distractor.id]
    getsGame (\g -> sourceCardCode g $ QuestSource $ toId old) `shouldReturn` Just (toCardCode Quests.manTheWallsAnnuminas)
    state <- getGame
    case publicGame state of
      Object fields -> for_ ["resolvingQuests", "frozenRoundBeginsAbilities", "epicQuestAdvance"] $ \key -> KeyMap.lookup key fields `shouldBe` Nothing
      _ -> error "invalid public game"
    savedQueue <- peekQueue
    generator <- asks gameRandomGen >>= readIORef
    restored <- either error pure $ eitherDecode $ encode state
    resumed <- liftIO $ newGameEnv restored (const $ pure ()) 0
    writeIORef (gameRandomGen resumed) generator
    liftIO $ runGameT resumed do
      setQueue savedQueue
      external $ CompleteQuest $ toId old
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.stopTheAttack
      getsGame (.outcome) `shouldReturn` Nothing
      external RevealEpicQuestAdvance
      resolveRevealed
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundRevealed
      external ReleaseEpicRoundBeginning
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundBegan
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.hostOfAngmar]
      resourceTotal `shouldReturn` 0
      getsGame (Map.null . gameResolvingQuests) `shouldReturn` True
      getsGame gameFrozenRoundBeginsAbilities `shouldReturn` Nothing
      external BeginEpicRoundResources
      resourceTotal `shouldReturn` 3
      getsGame (.round) `shouldReturn` 1

  for_ [Quests.guardTheGateAnnuminas, Quests.stopTheAttack] $ \priorDef ->
    it ("leaves a quest without defeating it and returns only a rotating stage-three physical card: " <> show (toCardCode priorDef)) . gameTest $ \_ -> do
      setReplica []
      _ <- putQuestStage priorDef
      prior <- getActiveQuest
      selected <- genEncounterCard Quests.leadTheSortie
      current <- getGame
      putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioQuestDeck = [selected]})
      setRoundPhase EpicRoundBeginReady
      external $ PrepareEpicQuestAdvance EpicLeaveQuest selected
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundPrepared
      (toCard <$> getActiveQuest) >>= (\card -> card.id `shouldBe` selected.id)
      getsGame (map (.id) . scenarioQuestDeck . toAttrs . (.scenario)) `shouldReturn` (if prior.stage == 3 then [(toCard prior).id] else [])
      getsGame (scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` []

  it "defaults newly added private beginning fields when loading an older game" . gameTest $ \_ -> do
    state <- getGame
    let legacy = case toJSON state of
          Object fields -> Object $ foldr KeyMap.delete fields ["frozenRoundBeginsAbilities", "resolvingQuests", "epicQuestAdvance"]
          value -> value
    restored <- either error pure $ eitherDecode $ encode legacy
    gameFrozenRoundBeginsAbilities restored `shouldBe` Nothing
    Map.null (gameResolvingQuests restored) `shouldBe` True
    gameEpicQuestAdvance restored `shouldBe` Nothing
