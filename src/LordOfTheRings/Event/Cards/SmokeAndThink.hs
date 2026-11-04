module LordOfTheRings.Event.Cards.SmokeAndThink (smokeAndThink) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Pipe))

newtype SmokeAndThink = SmokeAndThink EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

smokeAndThink :: EventCard SmokeAndThink
smokeAndThink = event SmokeAndThink Cards.smokeAndThink

-- | Action: Reduce the cost of the next card you play by 1 for each Pipe you control.
-- (The attachments on your characters are the ones you control.) It lasts until that card
-- is played, however long that is.
instance RunMessage SmokeAndThink where
  runMessage msg e@(SmokeAndThink attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      pipes <- length <$> (concatMapM (attachedWithTrait Pipe) =<< charactersControlledBy pid)
      when (pipes > 0) $ createEffect attrs (PlayerTarget pid) [LowerNextCostOf AnyCard pipes] UntilSpent
      pure e
    _ -> SmokeAndThink <$> runMessage msg attrs
