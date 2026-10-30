module LordOfTheRings.Quest.Cards.TheWeatherTurnsFoul (theWeatherTurnsFoul) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheWeatherTurnsFoul = TheWeatherTurnsFoul QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWeatherTurnsFoul :: QuestCard TheWeatherTurnsFoul
theWeatherTurnsFoul = quest TheWeatherTurnsFoul Cards.theWeatherTurnsFoul

-- | Sailing. Forced: At the end of the staging step, if you are off-course, reveal an
-- additional encounter card from the top of the encounter deck.
-- Winning as this stage is defeated with the Stormcaller at stage 3 is the scenario's.
instance HasAbilities TheWeatherTurnsFoul where
  getAbilities (TheWeatherTurnsFoul a) = [mkAbility a 1 $ forced StagingStepEndsWindow | a.flipped]

instance RunMessage TheWeatherTurnsFoul where
  runMessage msg q@(TheWeatherTurnsFoul attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      whenM isOffCourse revealEncounterCard
      pure q
    _ -> TheWeatherTurnsFoul <$> runMessage msg attrs
