module LordOfTheRings.Enemy.Cards.Rupert (rupert) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype Rupert = Rupert EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rupert :: EnemyCard Rupert
rupert = enemy Rupert Cards.rupert

-- | Forced: After Rupert attacks, shuffle all copies of the "Sacked!" card from the
-- discard pile back into the encounter deck.
instance HasAbilities Rupert where
  getAbilities (Rupert a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage Rupert where
  runMessage msg e@(Rupert attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck (cardIs Treacheries.sacked)
      pure e
    _ -> Rupert <$> runMessage msg attrs
