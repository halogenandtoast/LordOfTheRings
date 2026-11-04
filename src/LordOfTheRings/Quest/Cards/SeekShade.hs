module LordOfTheRings.Quest.Cards.SeekShade (seekShade) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (reduceTemperature, temperatureX)

newtype SeekShade = SeekShade QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
seekShade :: QuestCard SeekShade
seekShade = quest SeekShade Cards.seekShade

-- | Forced: At the end of the round, remove X progress from the main quest. X is the tens
-- digit of the temperature.
instance HasAbilities SeekShade where
  getAbilities (SeekShade a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | Response: When Seek Shade is defeated, reduce the temperature by 10. The players gain
-- by it, so it is taken, as Find Water's is.
instance RunMessage SeekShade where
  runMessage msg q@(SeekShade attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- temperatureX
      when (x > 0) $ removeProgressFromMainQuest attrs x
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      reduceTemperature 10
      SeekShade <$> runMessage msg attrs
    _ -> SeekShade <$> runMessage msg attrs
