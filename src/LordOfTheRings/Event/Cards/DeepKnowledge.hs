module LordOfTheRings.Event.Cards.DeepKnowledge (deepKnowledge) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype DeepKnowledge = DeepKnowledge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 2, on its card def. Action: Each player draws 2 cards.
deepKnowledge :: EventCard DeepKnowledge
deepKnowledge = event DeepKnowledge Cards.deepKnowledge

instance RunMessage DeepKnowledge where
  runMessage msg e@(DeepKnowledge attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> drawCards p 2
      pure e
    _ -> DeepKnowledge <$> runMessage msg attrs
