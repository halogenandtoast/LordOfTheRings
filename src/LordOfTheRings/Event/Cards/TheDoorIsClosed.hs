module LordOfTheRings.Event.Cards.TheDoorIsClosed (theDoorIsClosed) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheDoorIsClosed = TheDoorIsClosed EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After an encounter card is revealed from the encounter deck, cancel its
-- effects and discard it if there is a card with the same title in the victory display.
-- Answered before its keywords and When Revealed effects resolve, as A Test of Will.
theDoorIsClosed :: EventCard TheDoorIsClosed
theDoorIsClosed =
  eventWith TheDoorIsClosed Cards.theDoorIsClosed
    $ (windowL ?~ RevealedTitleInVictoryDisplayWindow When)
    . (criteriaL .~ EncounterEffectsCancelable)

instance RunMessage TheDoorIsClosed where
  runMessage msg e@(TheDoorIsClosed attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (take 1 [t | Window _ (Revealed t) <- windows]) \t -> do
        cancelWhenRevealed pid t
        -- what it keeps in play, its surge and doomed, goes with it as it is discarded
        cancelMatching \case
          FinishRevelation tid -> TreacheryTarget tid == t
          ResolveSurge t' _ -> t' == t
          _ -> False
        discard t
      pure e
    _ -> TheDoorIsClosed <$> runMessage msg attrs
