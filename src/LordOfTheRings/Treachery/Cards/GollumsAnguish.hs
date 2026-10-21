module LordOfTheRings.Treachery.Cards.GollumsAnguish (gollumsAnguish) where

import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard, passGollum, tantrumTargets)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GollumsAnguish = GollumsAnguish TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumsAnguish :: TreacheryCard GollumsAnguish
gollumsAnguish = treachery GollumsAnguish Cards.gollumsAnguish

-- | When Revealed: Raise the threat of the player guarding Gollum by 8. That player must
-- choose a new player to guard Gollum, if able.
-- Shadow: Raise the threat of the player guarding Gollum by 4.
instance RunMessage GollumsAnguish where
  runMessage msg t@(GollumsAnguish attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (`encounterRaisesThreat` 8) =<< tantrumTargets
      passGollum False
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      traverse_ (`encounterRaisesThreat` 4) =<< getGuard
      pure t
    _ -> GollumsAnguish <$> runMessage msg attrs
