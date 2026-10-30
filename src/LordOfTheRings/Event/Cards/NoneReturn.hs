module LordOfTheRings.Event.Cards.NoneReturn (noneReturn) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype NoneReturn = NoneReturn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Limit 3 copies of None Return in the victory display. Response: After a non-unique
-- enemy is destroyed, add None Return to the victory display to add that enemy to the
-- victory display, taken from the encounter discard pile.
noneReturn :: EventCard NoneReturn
noneReturn =
  eventWith NoneReturn Cards.noneReturn
    $ (windowL ?~ EnemyDestroyedWindow After (NotCard UniqueCard))
    . (criteriaL .~ FewerInVictoryDisplay 3 (cardIs Cards.noneReturn))

instance RunMessage NoneReturn where
  runMessage msg e@(NoneReturn attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (take 1 [c | Window _ (EnemyDestroyed c) <- windows]) \c -> do
        addToVictoryDisplay card
        addToVictoryDisplay c
      pure e
    _ -> NoneReturn <$> runMessage msg attrs
