module LordOfTheRings.Scenario.Scenarios.TheLongDark (theLongDark) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (passesLocateTest)

newtype TheLongDark = TheLongDark ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLongDark :: Scenario
theLongDark = Scenario $ scenario TheLongDark "the-long-dark" "The Long Dark"

instance RunMessage TheLongDark where
  runMessage msg (TheLongDark attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheLongDark, EncounterSet.TwistsAndTurns, EncounterSet.DeepsOfMoria]
      firstStage <- genEncounterCard Quests.journeyInTheBlackPit
      laterStages <- traverse genEncounterCard [Quests.continuingEastward]
      putQuestIntoPlay firstStage
      pure $ TheLongDark $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- the top card is discarded; the test is passed if it carries the pass icon
    LocateTest source onFail onPass -> case attrs.encounterDeck of
      top : _ -> do
        push $ DiscardTopOfEncounterDeck source GameTarget 1
        pushAll $ if passesLocateTest top then onPass else onFail
        pure $ TheLongDark attrs
      []
        | notNull attrs.encounterDiscard -> do
            deck <- shuffleM attrs.encounterDiscard
            push msg
            pure $ TheLongDark $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
        | otherwise -> do
            -- ponytail: nothing left to discard, so nothing is lost
            pushAll onPass
            pure $ TheLongDark attrs
    _ -> TheLongDark <$> runMessage msg attrs
