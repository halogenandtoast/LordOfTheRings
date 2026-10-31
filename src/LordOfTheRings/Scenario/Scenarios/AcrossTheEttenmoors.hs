module LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors (acrossTheEttenmoors) where

import LordOfTheRings.Campaign (PoolCard (..), addToPool)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Game.Base (gameRound)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (angmarSetup)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PhaseBegins))

newtype AcrossTheEttenmoors = AcrossTheEttenmoors ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

acrossTheEttenmoors :: Scenario
acrossTheEttenmoors = Scenario $ scenario AcrossTheEttenmoors "across-the-ettenmoors" "Across the Ettenmoors"

-- | The encounter deck is built from the Across the Ettenmoors, Eriador Wilds and Foul
-- Weather sets. The safe keyword's rules are kept by the safe locations themselves.
instance RunMessage AcrossTheEttenmoors where
  runMessage msg (AcrossTheEttenmoors attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.AcrossTheEttenmoors, EncounterSet.EriadorWilds, EncounterSet.FoulWeather]
      firstStage <- genEncounterCard Quests.intoTheEttenmoors
      laterStages <- traverse genEncounterCard [Quests.aMiserableJourney, Quests.journeysEnd]
      putQuestIntoPlay firstStage
      pure $ AcrossTheEttenmoors $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Campaign: Setup: Set all 5 Losing Time treacheries aside, out of play.
    DoStep 9 Setup | isJust attrs.campaign -> do
      angmarSetup attrs
      AcrossTheEttenmoors <$> runMessage msg attrs
    -- Forced: At the beginning of the quest phase, place 1 resource on this card. Then, if
    -- there are 3 or more resources on it, remove 3 resources from it to reveal 1 set-aside
    -- copy of Losing Time and place it in the encounter discard pile.
    -- ponytail: one resource a round, so every third round's quest phase reveals one, while
    -- any of the 5 are left; the resources are counted, not shown on the card. It answers
    -- the phase's window, before the players commit.
    CheckWindows ws | isJust attrs.campaign, Window After (PhaseBegins QuestPhase) `elem` ws -> do
      n <- getsGame gameRound
      when (n `mod` 3 == 0 && n `div` 3 <= 5) $ push . RevealCard =<< genEncounterCard Treacheries.losingTime
      AcrossTheEttenmoors <$> runMessage msg attrs
    -- Resolution: Add each copy of Losing Time in the encounter deck or encounter discard
    -- pile to the Campaign Pool.
    ResolveCampaign | Just l <- attrs.campaign -> do
      let losingTime = [c | c <- attrs.encounterDeck <> attrs.encounterDiscard, toCardCode c == toCardCode Treacheries.losingTime]
          l' = foldl' (\l'' c -> addToPool (PoolCard (toCardCode c) Nothing Nothing) l'') l losingTime
      AcrossTheEttenmoors <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> AcrossTheEttenmoors <$> runMessage msg attrs
