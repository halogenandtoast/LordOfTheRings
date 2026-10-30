module LordOfTheRings.Treachery.Cards.DarkSorcery (darkSorcery) where

import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (copyInOwnersDiscard)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarkSorcery = DarkSorcery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkSorcery :: TreacheryCard DarkSorcery
darkSorcery = treachery DarkSorcery Cards.darkSorcery

-- | Doomed 2.
-- When Revealed: Each player discards each ally he controls that has the same title as a
-- card in his discard pile. The pile is read as the effect begins: an ally it discards
-- discards no other.
-- Shadow: Discard the defending character if there is a copy of that character in its
-- owner's discard pile.
instance RunMessage DarkSorcery where
  runMessage msg t@(DarkSorcery attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- select AnyAlly
      copied <- filterM (copyInOwnersDiscard . toTarget) allies
      traverse_ discard copied
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      traverse_ discard =<< filterM copyInOwnersDiscard defenders
      pure t
    _ -> DarkSorcery <$> runMessage msg attrs
