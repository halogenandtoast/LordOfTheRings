module LordOfTheRings.Treachery.Cards.SouthronSupport (southronSupport) where

import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Harad))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SouthronSupport = SouthronSupport TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronSupport :: TreacheryCard SouthronSupport
southronSupport = treachery SouthronSupport Cards.southronSupport

-- | Doomed 3.
-- When Revealed: Each player must search the encounter deck and discard pile for 1 Harad
-- enemy and add it to the staging area, if able. Shuffle the encounter deck.
instance RunMessage SouthronSupport where
  runMessage msg t@(SouthronSupport attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscard pid attrs (CardWithType EnemyType <> CardWithTrait Harad)
      pure t
    _ -> SouthronSupport <$> runMessage msg attrs
