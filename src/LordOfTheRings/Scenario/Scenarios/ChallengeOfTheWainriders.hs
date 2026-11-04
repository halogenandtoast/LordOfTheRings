{- | Challenge of the Wainriders. The players race The Wainriders around the circuit, its six
stages laid out as a loop: see "LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers".
The scenario keeps the circuit, advances The Challengers and The Wainriders around it,
places the damage on The Wainriders' stage, resolves racing tests, and ends the game once
either has completed the circuit twice.
-}
module LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders (challengeOfTheWainriders) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (CannotExhaust, RacingTestAdditionalResults))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Window (WindowType (AdvancedToStage), mkAfter)

newtype ChallengeOfTheWainriders = ChallengeOfTheWainriders ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

challengeOfTheWainriders :: Scenario
challengeOfTheWainriders = Scenario $ scenario ChallengeOfTheWainriders "challenge-of-the-wainriders" "Challenge of the Wainriders"

{- | The encounter deck is built from the Challenge of the Wainriders, Rolling Plains and
Riders of Rhûn sets (MEC80 rules). Stage 1A's setup takes The Wainriders, The Challengers
and Wainrider Champion from those set aside.
-}
instance RunMessage ChallengeOfTheWainriders where
  runMessage msg (ChallengeOfTheWainriders attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.ChallengeOfTheWainriders, EncounterSet.RollingPlains, EncounterSet.RidersOfRhun]
      let (aside, deck) =
            partition (cardMatches $ CardMatchAny [cardIs Objectives.theWainriders, cardIs Objectives.theChallengers, cardIs Enemies.wainriderChampion]) cards
      -- stages 1, 2 and 3 in a row, 4, 5 and 6 above them from right to left: the circuit
      stages <- traverse genEncounterCard [Quests.wainriderCamp, Quests.firstSprint, Quests.theTurnaround, Quests.theFarthestEdge, Quests.lastClimb, Quests.homeStretch]
      for_ (take 1 stages) putQuestIntoPlay
      pure $ withRace (Circuit stages 0 0 0 0 Nothing) (attrs & encounterDeckL .~ deck & setAsideL .~ aside)
    -- the first player commits a single character to the test by exhausting it
    ScenarioSpecific "racing.test" (parseMaybe parseJSON -> Just test) -> do
      lead <- getLead
      readied <- selectCharacters (HeroControlledBy (PlayerWithId lead) <> ReadyHero) (AllyControlledBy (PlayerWithId lead) <> ReadyAlly)
      candidates <- filterM (fmap (notElem CannotExhaust) . getModifiers) readied
      if null candidates
        then push $ ScenarioSpecific "racing.commit" (toJSON (Nothing :: Maybe Target))
        else chooseOneM lead do
          questionLabeledWith (statKey test.racingStat) [intVar "required" test.racingRequired]
          targets candidates \c -> do
            exhaust c
            push $ ScenarioSpecific "racing.commit" (toJSON (Just c))
      pure $ withRace st {pendingTest = Just test} attrs
    -- then discards the top X cards of the encounter deck, X the character's attribute
    ScenarioSpecific "racing.commit" (parseMaybe parseJSON -> Just committed) | Just test <- st.pendingTest -> do
      x <- maybe (pure 0) (statOf test.racingStat) committed
      discardTopOfEncounterDeckFor ScenarioSource ScenarioTarget x
      pure $ ChallengeOfTheWainriders attrs
    DiscardedTopOfEncounterDeck ScenarioSource ScenarioTarget discarded | Just test <- st.pendingTest -> do
      mods <- getModifiers GameTarget
      let required = test.racingRequired + sum [k | RacingTestAdditionalResults k <- mods]
          found = racingResults test.racingResult discarded
          passed = found >= required
      push $ ScenarioSpecific "racing.result" (toJSON (found, required, passed))
      pushAll $ if passed then test.racingOnPass else test.racingOnFail
      ChallengeOfTheWainriders <$> runMessage msg (withRaceAttrs st {pendingTest = Nothing} attrs)
    -- The Challengers advance counterclockwise to the next stage, the current quest; the
    -- stage they leave is in the circuit again, its progress gone
    ScenarioSpecific "challengers.advance" (parseMaybe parseJSON -> Just (excess :: Maybe Int)) -> do
      mains <- getMainQuests
      for_ [qid | QuestTarget qid <- mains] (push . LeaveQuestStage)
      let n = st.challengers + 1
      if n >= 12
        then gameOver Won
        else for_ (stageOfCircuit st n) \next -> do
          push $ ScenarioSpecific "challengers.advanced" (toJSON next)
          putQuestIntoPlayFlipped next
          push $ CheckWindows [mkAfter AdvancedToStage]
          for_ excess \k -> push $ ScenarioSpecific "race.excess" (toJSON k)
      pure $ withRace st {challengers = n, completedThisRound = st.completedThisRound + 1} attrs
    ScenarioSpecific "race.excess" (parseMaybe parseJSON -> Just (k :: Int)) -> do
      when (k > 0) $ placeProgressOnMainQuest ScenarioSource k
      pure $ ChallengeOfTheWainriders attrs
    -- damage on The Wainriders' stage: with as much as its quest points The Wainriders
    -- advance, the excess placed on the next stage, which may advance them again
    ScenarioSpecific "wainriders.damage" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      points <- wainridersStageQuestPoints
      let total = max 0 (st.wainridersDamage + n)
          advances = if points > 0 then total `div` points else 0
          reached = st.wainriders + advances
          passed = [st.wainriders + 1 .. reached]
      for_ passed \k -> for_ (stageOfCircuit st k) (push . ScenarioSpecific "wainriders.advanced" . toJSON)
      when (reached >= 12) $ gameOver Lost
      pure $ withRace st {wainriders = reached, wainridersDamage = total - advances * points} attrs
    EndRound -> ChallengeOfTheWainriders <$> runMessage msg (withRaceAttrs st {completedThisRound = 0} attrs)
    _ -> ChallengeOfTheWainriders <$> runMessage msg attrs
   where
    st = raceOf attrs
    withRace r a = ChallengeOfTheWainriders $ withRaceAttrs r a
    withRaceAttrs r a = a & metaL .~ toJSON r
    statKey = \case
      EscapeWillpower -> "racingTest.willpower"
      EscapeAttack -> "racingTest.attack"
      EscapeDefense -> "racingTest.defense"
    statOf = \case
      EscapeWillpower -> getWillpower
      EscapeAttack -> getAttack
      EscapeDefense -> getDefense
