module LordOfTheRings.Event.Cards.LeaveNoTrace (leaveNoTrace) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype LeaveNoTrace = LeaveNoTrace EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Limit 3 copies of Leave No Trace in the victory display. Response: After a
-- non-unique location is explored, add Leave No Trace to the victory display to add that
-- location to the victory display. Answered as the explored location leaves play, as
-- Ancient Mathom answers, so it goes to the victory display in place of the discard pile.
leaveNoTrace :: EventCard LeaveNoTrace
leaveNoTrace =
  eventWith LeaveNoTrace Cards.leaveNoTrace
    $ (windowL ?~ ExploredMatchingWindow When (NotCard UniqueCard))
    . (criteriaL .~ FewerInVictoryDisplay 3 (cardIs Cards.leaveNoTrace))

instance RunMessage LeaveNoTrace where
  runMessage msg e@(LeaveNoTrace attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (take 1 [lid | Window _ (Explored lid) <- windows]) \lid -> do
        addToVictoryDisplay card
        addToVictoryDisplay (LocationTarget lid)
      pure e
    _ -> LeaveNoTrace <$> runMessage msg attrs
