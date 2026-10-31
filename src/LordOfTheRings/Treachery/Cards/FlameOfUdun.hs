module LordOfTheRings.Treachery.Cards.FlameOfUdun (flameOfUdun) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FlameOfUdun = FlameOfUdun TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Shadow and Flame's Balrog deck, in nightmare mode.
flameOfUdun :: TreacheryCard FlameOfUdun
flameOfUdun = treachery FlameOfUdun Cards.flameOfUdun

-- | Shadow: Deal Durin's Bane 3 additional shadow cards from the encounter deck (not the
-- Balrog deck).
instance RunMessage FlameOfUdun where
  runMessage msg t@(FlameOfUdun attrs) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      dealShadowCardsFromEncounterDeckDuringAttack eid pid defenders (take 3 deck)
      pure t
    _ -> FlameOfUdun <$> runMessage msg attrs
