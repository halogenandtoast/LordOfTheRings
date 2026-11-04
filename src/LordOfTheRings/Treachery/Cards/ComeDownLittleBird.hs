module LordOfTheRings.Treachery.Cards.ComeDownLittleBird (comeDownLittleBird) where

import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Creature))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ComeDownLittleBird = ComeDownLittleBird TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

comeDownLittleBird :: TreacheryCard ComeDownLittleBird
comeDownLittleBird = treachery ComeDownLittleBird Cards.comeDownLittleBird

-- | When Revealed: Starting with the first player, each player must search the encounter
-- deck and encounter discard pile for 1 Creature enemy and put it into play engaged with
-- him. Then, shuffle the encounter deck.
-- (Its riddle is answered by the scenario.)
instance RunMessage ComeDownLittleBird where
  runMessage msg t@(ComeDownLittleBird attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckAndDiscardToEngage p attrs (CardWithType EnemyType <> CardWithTrait Creature)
      shuffleEncounterDeck
      pure t
    _ -> ComeDownLittleBird <$> runMessage msg attrs
