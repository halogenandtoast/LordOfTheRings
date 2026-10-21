module LordOfTheRings.Treachery.Cards.WastedProvisions (wastedProvisions) where

import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard, passGollum, tantrumTargets)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WastedProvisions = WastedProvisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wastedProvisions :: TreacheryCard WastedProvisions
wastedProvisions = treachery WastedProvisions Cards.wastedProvisions

-- | When Revealed: Discard the top 10 cards from the deck of the player guarding Gollum.
-- That player must choose a new player to guard Gollum, if able.
-- Shadow: Discard the top 5 cards from the deck of the player guarding Gollum.
instance RunMessage WastedProvisions where
  runMessage msg t@(WastedProvisions attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (`discardTopOfDeck` 10) =<< tantrumTargets
      passGollum False
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      traverse_ (`discardTopOfDeck` 5) =<< getGuard
      pure t
    _ -> WastedProvisions <$> runMessage msg attrs
