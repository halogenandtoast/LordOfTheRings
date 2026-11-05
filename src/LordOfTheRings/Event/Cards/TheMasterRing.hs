module LordOfTheRings.Event.Cards.TheMasterRing (theMasterRing) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheMasterRing = TheMasterRing EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: Exhaust The One Ring and raise your threat by 1 to cancel the effects of an
-- encounter card just revealed from the encounter deck and discard that card. Then, reveal
-- an encounter card. Answered before its keywords and When Revealed effects resolve, as
-- The Door is Closed!.
theMasterRing :: EventCard TheMasterRing
theMasterRing =
  eventWith TheMasterRing Cards.theMasterRing \a ->
    a
      & (windowL ?~ EncounterCardRevealedWindow When)
      & (criteriaL .~ (EncounterEffectsCancelable <> theOneRingReady a.owner))

instance RunMessage TheMasterRing where
  runMessage msg e@(TheMasterRing attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      payTheOneRing pid
      for_ (take 1 [t | Window _ (Revealed t) <- windows]) \t -> do
        cancelWhenRevealed pid t
        -- what it keeps in play, its surge and doomed, goes with it as it is discarded
        cancelMatching \case
          FinishRevelation tid -> TreacheryTarget tid == t
          ResolveSurge t' _ -> t' == t
          _ -> False
        discard attrs t
        revealEncounterCardAfter t
      pure e
    _ -> TheMasterRing <$> runMessage msg attrs
