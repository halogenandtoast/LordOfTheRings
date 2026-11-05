module LordOfTheRings.LostRealmEngineSpec (spec) where

import LordOfTheRings.Criteria (Criterion (FewerInVictoryDisplay))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Entities (treacheriesL)
import LordOfTheRings.Helpers.Window (passesCriteria, windowMatchesM)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (
  currentQuestIsSideQuest,
  getCurrentQuest,
  getEncounterSideQuests,
  getSideQuests,
  getTimeCounters,
  progressPlacedThisRound,
  questCardsInPlay,
 )
import LordOfTheRings.Matcher.Types (cardIs)
import LordOfTheRings.Matcher.Window (WindowMatcher (SideQuestRevealedWindow, TreacheryWithTraitRevealedWindow), windowMatches)
import LordOfTheRings.Message.Lifted (
  chooseNewCurrentQuest,
  chooseOrRunOneM,
  labeled,
  raiseThreat,
  removeProgressFromEachQuest,
  valourLabeled,
 )
import LordOfTheRings.Modifier (ModifierType (CannotBeCurrentQuest, ProgressPerRoundLimit))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Hazard, Sorcery))
import LordOfTheRings.Treachery (newTreachery)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
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
      run $ Discard GameSource side
      getSideQuests `shouldReturn` []
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Quests.searchTheRuins]

  describe "Progress on quest cards" do
    it "is counted each round, and kept to a quest card's limit for the round" . gameTest $ \_ -> do
      main <- putQuestStage Quests.fliesAndSpiders
      run $ CreateEffect $ Effect GameSource main [ProgressPerRoundLimit 3] UntilEndOfRound
      run $ PlaceTokens GameSource main Progress 2
      run $ PlaceTokens GameSource main Progress 2
      progressOn main `shouldReturn` 3
      progressPlacedThisRound main `shouldReturn` 3

    it "is removed from each quest card in play" . gameTest $ \_ -> do
      (main, side) <- withSideQuest
      run $ PlaceTokens GameSource main Progress 1
      run $ PlaceTokens GameSource side Progress 3
      runQueueT $ removeProgressFromEachQuest GameSource 2
      void mainLoop
      progressOn main `shouldReturn` 0
      progressOn side `shouldReturn` 1

  describe "Time counters" do
    it "are not placed again as a separated area comes back into focus" . twoPlayerTest testDeck $ \one two -> do
      run $ SeparatePlayers [[one], [two]]
      run . AddToStaging =<< genEncounterCard Locations.ravenChiefsCamp
      camp <- getsGame (toTarget . fromJustNote "no camp" . headMay . toList . (.entities.locations))
      getTimeCounters camp `shouldReturn` 3
      run $ FocusArea two
      run $ FocusArea one
      getTimeCounters camp `shouldReturn` 3

  describe "Revealed treacheries" do
    it "open the window for a treachery of the trait" . gameTest $ \_ -> do
      t <- newTreachery =<< genEncounterCard Treacheries.caveIn
      putGame . (entitiesL . treacheriesL %~ insertEntity t) =<< getGame
      windowMatchesM (TreacheryWithTraitRevealedWindow After Hazard) (mkAfter $ Revealed $ toTarget t) `shouldReturn` True
      windowMatchesM (TreacheryWithTraitRevealedWindow After Sorcery) (mkAfter $ Revealed $ toTarget t) `shouldReturn` False

  describe "Valour" do
    let trigger pid = runQueueT $ chooseOrRunOneM pid do
          labeled "questAgainstMainQuest" $ raiseThreat pid 1
          valourLabeled pid $ raiseThreat pid 5
    it "resolves a card's other trigger without asking below 40 threat" . gameTest $ \pid -> do
      threat0 <- threatOf pid
      trigger pid
      void mainLoop
      threatOf pid `shouldReturn` threat0 + 1

    it "offers the Valour trigger at 40 threat or higher" . gameTest $ \pid -> do
      run . RaiseThreat pid . (40 -) =<< threatOf pid
      trigger pid
      void mainLoop
      chooseLabel "$label.valour" pid
      threatOf pid `shouldReturn` 45

  describe "The victory display" do
    it "takes a card from the encounter discard pile" . gameTest $ \_ -> do
      card <- genEncounterCard Locations.oldForestRoad
      run $ AddToEncounterDiscard [card]
      run $ AddToVictoryDisplay (CardIdTarget card.id)
      victoryDisplay `shouldReturn` [toCardCode Locations.oldForestRoad]
      getEncounterDiscard `shouldReturn` []
      passesCriteria (FewerInVictoryDisplay 1 $ cardIs Locations.oldForestRoad) `shouldReturn` False
      passesCriteria (FewerInVictoryDisplay 2 $ cardIs Locations.oldForestRoad) `shouldReturn` True
