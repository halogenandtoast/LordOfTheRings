module LordOfTheRings.Treachery.Cards.ManyRoads (manyRoads) where

import LordOfTheRings.Card.CardType (CardType (LocationType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ManyRoads = ManyRoads TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

manyRoads :: TreacheryCard ManyRoads
manyRoads = treachery ManyRoads Cards.manyRoads

-- | Surge.
-- When Revealed: Shuffle all locations in the encounter discard pile back into the
-- encounter deck.
instance RunMessage ManyRoads where
  runMessage msg t@(ManyRoads attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck (CardWithType LocationType)
      pure t
    _ -> ManyRoads <$> runMessage msg attrs
