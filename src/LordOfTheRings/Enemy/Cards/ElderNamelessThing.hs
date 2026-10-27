module LordOfTheRings.Enemy.Cards.ElderNamelessThing (elderNamelessThing) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (attachFromTopOfDeck, namelessX)

newtype ElderNamelessThing = ElderNamelessThing EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

elderNamelessThing :: EnemyCard ElderNamelessThing
elderNamelessThing = enemy ElderNamelessThing Cards.elderNamelessThing

-- | X is the printed cost of all attached cards on this card. If there are no cards
-- attached, X is 4.
instance HasModifiersFor ElderNamelessThing where
  getModifiersFor (ElderNamelessThing a) = let x = namelessX 4 a in modified a a [AttackModifier x, HitPointModifier x]

-- | Forced: After Elder Nameless Thing engages a player, attach the top 3 cards of that
-- player's deck to it.
instance HasAbilities ElderNamelessThing where
  getAbilities (ElderNamelessThing a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage ElderNamelessThing where
  runMessage msg e@(ElderNamelessThing attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> attachFromTopOfDeck pid 3 attrs
      pure e
    _ -> ElderNamelessThing <$> runMessage msg attrs
