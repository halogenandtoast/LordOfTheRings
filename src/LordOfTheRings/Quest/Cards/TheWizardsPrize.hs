module LordOfTheRings.Quest.Cards.TheWizardsPrize (theWizardsPrize) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (getMugash, mugashHost)

newtype TheWizardsPrize = TheWizardsPrize QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWizardsPrize :: QuestCard TheWizardsPrize
theWizardsPrize = quest TheWizardsPrize Cards.theWizardsPrize

-- | Time 3.
-- Forced: After the last time counter is removed from this stage, return Mugash to the
-- staging area and reveal X encounter cards. X is the number of players in the game minus
-- 1. Place 3 time counters on this stage.
instance HasAbilities TheWizardsPrize where
  getAbilities (TheWizardsPrize a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Progress cannot be placed on this stage unless Mugash is attached to a hero.
-- If the players defeat this stage, they win the game (the quest deck is empty).
instance RunMessage TheWizardsPrize where
  runMessage msg q@(TheWizardsPrize attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      mugash <- getMugash
      for_ mugash \m -> when (m.placement /= StagingArea) $ placeEnemy m.id StagingArea
      players <- getRemainingPlayers
      revealEncounterCards (length players - 1)
      placeTimeCounters attrs attrs 3
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      captured <- isJust <$> mugashHost
      if captured then TheWizardsPrize <$> runMessage msg attrs else pure q
    _ -> TheWizardsPrize <$> runMessage msg attrs
