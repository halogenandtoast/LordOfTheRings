module LordOfTheRings.Treachery.Cards.LureOfTheRing (lureOfTheRing) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing, theOneRingExhausted)
import LordOfTheRings.Treachery.Import.Lifted

newtype LureOfTheRing = LureOfTheRing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
lureOfTheRing :: TreacheryCard LureOfTheRing
lureOfTheRing = treachery LureOfTheRing Cards.lureOfTheRing

-- | When Revealed: The first player must either exhaust The One Ring or raise his threat by
-- 3: with The One Ring already exhausted, he raises it.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if The One Ring is exhausted).
instance RunMessage LureOfTheRing where
  runMessage msg t@(LureOfTheRing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      exhausted <- theOneRingExhausted
      ring <- getSagaOneRing
      chooseOrRunOneM lead do
        questionLabeled "lureOfTheRing.choose"
        causedBy attrs
        unless exhausted $ for_ ring \r -> labeled "lureOfTheRing.exhaust" $ exhaust (ObjectiveTarget r.id)
        labeled "lureOfTheRing.raiseThreat" $ encounterRaisesThreat lead 3
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      exhausted <- theOneRingExhausted
      enemyGetsAttack (CardIdSource cid) eid (if exhausted then 2 else 1)
      pure t
    _ -> LureOfTheRing <$> runMessage msg attrs
