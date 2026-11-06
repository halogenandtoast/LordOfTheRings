module LordOfTheRings.Enemy.Cards.RestlessDead (restlessDead) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Undead))

newtype RestlessDead = RestlessDead EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

restlessDead :: EnemyCard RestlessDead
restlessDead = enemy RestlessDead Cards.restlessDead

instance RunMessage RestlessDead where
  runMessage msg e@(RestlessDead a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      pile <- getEncounterDiscard
      traverse_ addToStagingFromDiscard $ find (cardMatches $ CardWithType EnemyType <> CardWithTrait Undead) pile
      pure e
    _ -> RestlessDead <$> runMessage msg a
