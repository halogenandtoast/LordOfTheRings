module LordOfTheRings.Treachery.Cards.DaechanarsCurse (daechanarsCurse) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateTopOfDeck)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DaechanarsCurse = DaechanarsCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

daechanarsCurse :: TreacheryCard DaechanarsCurse
daechanarsCurse = treachery DaechanarsCurse Cards.daechanarsCurse

-- | Surge.
-- When Revealed: Attach Daechanar's Curse to the top of the first player's deck. (Counts as
-- a Condition attachment with the text: "The next time a player would draw 1 or more cards
-- from attached deck, reanimate those cards instead. Then, discard Daechanar's Curse.")
-- On the player, it is discarded in place of his next draw, as a condition on top of his
-- deck is; it reanimates the cards that draw would take.
instance RunMessage DaechanarsCurse where
  runMessage msg t@(DaechanarsCurse attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      attachTreachery attrs.treacheryId (PlayerTarget lead)
      pure t
    DrawCards drawing | AttachedTo (PlayerTarget pid) <- attrs.placement, drawing.player == pid -> do
      reanimateTopOfDeck pid drawing.count
      DaechanarsCurse <$> runMessage msg attrs
    _ -> DaechanarsCurse <$> runMessage msg attrs
