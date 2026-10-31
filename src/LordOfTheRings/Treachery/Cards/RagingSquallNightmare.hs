module LordOfTheRings.Treachery.Cards.RagingSquallNightmare (ragingSquallNightmare) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RagingSquallNightmare = RagingSquallNightmare TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ragingSquallNightmare :: TreacheryCard RagingSquallNightmare
ragingSquallNightmare = treachery RagingSquallNightmare Cards.ragingSquallNightmare

-- | Surge.
-- *Forced: After Raging Squall is discarded from the encounter deck during a Sailing test,
-- deal 1 damage to each character committed to that test.
instance RunMessage RagingSquallNightmare where
  runMessage msg t@(RagingSquallNightmare attrs) = case msg of
    ResolveSailingDiscard card committed | card.id == attrs.cardId -> do
      for_ committed \c -> dealDamage attrs c 1
      pure t
    _ -> RagingSquallNightmare <$> runMessage msg attrs
