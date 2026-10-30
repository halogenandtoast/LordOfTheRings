module LordOfTheRings.LostRealmEngineSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (
  currentQuestIsSideQuest,
  getCurrentQuest,
  getEncounterSideQuests,
  getSideQuests,
  questCardsInPlay,
 )
import LordOfTheRings.Matcher.Window (WindowMatcher (SideQuestRevealedWindow), windowMatches)
import LordOfTheRings.Message.Lifted (
  chooseNewCurrentQuest,
 )
import LordOfTheRings.Modifier (ModifierType (CannotBeCurrentQuest))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Window (Timing (After), WindowType (PhaseEnds, Revealed), mkAfter, mkWhen)
import TestImport

-- | Flies and Spiders as the main quest, and Search the Ruins revealed from the encounter
-- deck, the Old Forest Road under it for its surge.
withSideQuest :: GameT (Target, Target)
withSideQuest = do
  main <- putQuestStage Quests.fliesAndSpiders
  _ <- setEncounterDeck [Quests.searchTheRuins, Locations.oldForestRoad]
  run RevealEncounterCard
  side <- getsGame (toTarget . fromJustNote "no side quest" . find ((== toCardCode Quests.searchTheRuins) . toCardCode) . toList . (.entities.quests))
  pure (main, side)

progressOn :: Target -> GameT Int
progressOn t = getsGame (maybe 0 (.progress) . find ((== t) . toTarget) . toList . (.entities.quests))

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

spec :: Spec
spec = describe "The Lost Realm engine" do
  describe "Encounter side quests" do
    it "are revealed into the staging area, and surge as their keyword says" . gameTest $ \_ -> do
      (main, side) <- withSideQuest
      getSideQuests `shouldReturn` [side]
      getEncounterSideQuests `shouldReturn` [side]
      questCardsInPlay `shouldReturn` 2
      getCurrentQuest `shouldReturn` Just main
      -- the surge revealed the Old Forest Road
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.oldForestRoad]

    it "may be chosen as the current quest in the quest phase" . gameTest $ \pid -> do
      (_, side) <- withSideQuest
      run $ BeginPhase QuestPhase
      chooseTarget pid side
      getCurrentQuest `shouldReturn` Just side
      currentQuestIsSideQuest `shouldReturn` True

    it "take the progress as the current quest, and go to the victory display once defeated" . gameTest $ \pid -> do
      (main, side) <- withSideQuest
      run $ RaiseThreat pid 10
      run $ SetCurrentQuest side
      threat0 <- threatOf pid
      run $ PlaceProgressOnQuest 20
      -- Search the Ruins' response lowers each player's threat by 3
      threatOf pid `shouldReturn` threat0 - 3
      victoryDisplay `shouldReturn` [toCardCode Quests.searchTheRuins]
      getEncounterSideQuests `shouldReturn` []
      getCurrentQuest `shouldReturn` Just main
      progressOn main `shouldReturn` 0

    it "keep their text active in the staging area" . gameTest $ \pid -> do
      _ <- withSideQuest
      threat0 <- threatOf pid
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      threatOf pid `shouldReturn` threat0 + 2

    it "are not offered as the current quest when they cannot be, nor is the main quest" . gameTest $ \pid -> do
      (main, side) <- withSideQuest
      run $ CreateEffect $ Effect GameSource main [CannotBeCurrentQuest] UntilEndOfRound
      run $ BeginPhase QuestPhase
      Just question <- getQuestion pid
      [() | Label "$label.questAgainstMainQuest" _ <- questionChoices question] `shouldBe` []
      [t | TargetLabel t _ <- questionChoices question] `shouldBe` [side]

    it "give way to another quest card a card chooses as the current quest" . gameTest $ \pid -> do
      (main, side) <- withSideQuest
      run $ SetCurrentQuest side
      runQueueT $ chooseNewCurrentQuest pid GameTarget
      void mainLoop
      Just question <- getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldBe` [main]
      chooseTarget pid main
      getCurrentQuest `shouldReturn` Just main

    it "open the window for a side quest revealed" . gameTest $ \_ -> do
      (_, side) <- withSideQuest
      windowMatches (SideQuestRevealedWindow After) (mkAfter $ Revealed side) `shouldBe` True
      windowMatches (SideQuestRevealedWindow After) (mkAfter $ Revealed GameTarget) `shouldBe` False

    it "can be discarded or removed from the game" . gameTest $ \_ -> do
      (_, side) <- withSideQuest
      run $ Discard side
      getSideQuests `shouldReturn` []
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Quests.searchTheRuins]
