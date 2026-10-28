module LordOfTheRings.Treachery.Cards.MordorLooms (mordorLooms) where

import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MordorLooms = MordorLooms TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mordorLooms :: TreacheryCard MordorLooms
mordorLooms = treachery MordorLooms Cards.mordorLooms

-- | Surge.
-- When Revealed: Each player takes 1 hidden card.
-- Shadow: If this attack destroys a character, that character's controller turns each of
-- his hidden cards faceup. The shadow marks the attack with an effect from its card, which
-- the scenario reads as the attack's damage is dealt.
instance RunMessage MordorLooms where
  runMessage msg t@(MordorLooms attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer takeHiddenCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      createEffect (CardIdSource cid) eid [] UntilEndOfAttack
      pure t
    _ -> MordorLooms <$> runMessage msg attrs
