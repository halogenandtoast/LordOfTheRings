module LordOfTheRings.Event.Cards.StandTogether (standTogether) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype StandTogether = StandTogether EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a player. That player may declare any number of his eligible
-- characters as defenders against each enemy attacking him this phase.
standTogether :: EventCard StandTogether
standTogether = event StandTogether Cards.standTogether

instance RunMessage StandTogether where
  runMessage msg e@(StandTogether attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> phaseModifier attrs p MayDeclareMultipleDefenders
      pure e
    _ -> StandTogether <$> runMessage msg attrs
