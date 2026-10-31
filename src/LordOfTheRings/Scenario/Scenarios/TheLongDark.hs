module LordOfTheRings.Scenario.Scenarios.TheLongDark (theLongDark) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Matcher (getLead, getPlayer)
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (sourceTarget)
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (passesLocateTest)

newtype TheLongDark = TheLongDark ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
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
    -- The first player may discard a card from his hand to discard the top card of the
    -- encounter deck, again and again until one carries the pass icon. Once he runs out
    -- of cards or stops, the test is failed.
    LocateTest source onFail _ -> do
      lead <- getLead
      hand <- discardableHand <$> getPlayer lead
      chooseOrRunOneM lead do
        questionLabeled "locateTest.discard"
        traverse_ causedBy (sourceTarget source)
        targets hand \c -> do
          discardFromHand lead c
          push $ DoStep 1 msg
        labeled "locateTest.fail" $ pushAll onFail
      pure $ TheLongDark attrs
    DoStep 1 test@(LocateTest source _ onPass) -> case attrs.encounterDeck of
      top : _ -> do
        push $ DiscardTopOfEncounterDeck source GameTarget 1
        if passesLocateTest top then pushAll onPass else push test
        pure $ TheLongDark attrs
      []
        | notNull attrs.encounterDiscard -> do
            deck <- shuffleM attrs.encounterDiscard
            push msg
            pure $ TheLongDark $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
        | otherwise -> do
            -- nothing is discarded, so no pass icon either
            push test
            pure $ TheLongDark attrs
    _ -> TheLongDark <$> runMessage msg attrs
