module LordOfTheRings.Treachery.Cards.ExhalationOfDecay (exhalationOfDecay) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ExhalationOfDecay = ExhalationOfDecay TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

exhalationOfDecay :: TreacheryCard ExhalationOfDecay
exhalationOfDecay = treachery ExhalationOfDecay Cards.exhalationOfDecay

-- | When Revealed: Each player discards 1 random card from his hand and 1 non-objective
-- attachment he controls.
-- Shadow: Discard a non-objective attachment you control.
instance RunMessage ExhalationOfDecay where
  runMessage msg t@(ExhalationOfDecay attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        discardRandomFromHand p 1
        discardNonObjectiveAttachment attrs p
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure t
    _ -> ExhalationOfDecay <$> runMessage msg attrs
