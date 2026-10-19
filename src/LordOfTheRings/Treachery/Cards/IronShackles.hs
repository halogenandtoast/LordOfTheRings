module LordOfTheRings.Treachery.Cards.IronShackles (ironShackles) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype IronShackles = IronShackles TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ironShackles :: TreacheryCard IronShackles
ironShackles = treachery IronShackles Cards.ironShackles

-- | When Revealed: Attach Iron Shackles to the top of the first player's deck. (Counts as a
-- Condition attachment with the text: "The next time a player would draw 1 or more cards
-- from attached deck, discard Iron Shackles instead.")
-- Shadow: Resolve the "When Revealed" effect of Iron Shackles.
instance RunMessage IronShackles where
  runMessage msg t@(IronShackles attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      attachTreachery attrs.treacheryId (PlayerTarget lead)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      lead <- getLead
      push $ PutShadowTreacheryIntoPlay eid cid (AttachedTo $ PlayerTarget lead)
      pure t
    _ -> IronShackles <$> runMessage msg attrs
