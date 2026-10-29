module LordOfTheRings.Event.Cards.FollowMe (followMe) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype FollowMe = FollowMe EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Take control of the first player token and draw 1 card.
followMe :: EventCard FollowMe
followMe = event FollowMe Cards.followMe

instance RunMessage FollowMe where
  runMessage msg e@(FollowMe attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      push $ SetFirstPlayer pid
      drawCards attrs pid 1
      pure e
    _ -> FollowMe <$> runMessage msg attrs
