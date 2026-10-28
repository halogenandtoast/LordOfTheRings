module LordOfTheRings.Event.Cards.TrainedForWar (trainedForWar) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype TrainedForWar = TrainedForWar EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if each hero you control has the printed tactics resource icon.
-- Action: Until the end of the phase, if the current quest does not have Siege it gains
-- Battle.
trainedForWar :: EventCard TrainedForWar
trainedForWar = eventWith TrainedForWar Cards.trainedForWar \a -> a & criteriaL .~ eachHeroHasPrintedIcon a.owner Tactics

instance RunMessage TrainedForWar where
  runMessage msg e@(TrainedForWar attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      siege <- currentQuestHas Siege
      quest <- getCurrentQuest
      for_ quest \q -> unless siege $ phaseModifier attrs q (GainKeyword Battle)
      pure e
    _ -> TrainedForWar <$> runMessage msg attrs
