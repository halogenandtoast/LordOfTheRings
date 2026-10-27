module LordOfTheRings.Enemy.Cards.NamelessThing (namelessThing) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (attachFromTopOfDeck, namelessX)

newtype NamelessThing = NamelessThing EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

namelessThing :: EnemyCard NamelessThing
namelessThing = enemy NamelessThing Cards.namelessThing

-- | X is the printed cost of all attached cards on this card. If there are no cards
-- attached, X is 3.
instance HasModifiersFor NamelessThing where
  getModifiersFor (NamelessThing a) = let x = namelessX 3 a in modified a a [AttackModifier x, HitPointModifier x]

-- | Forced: After Nameless Thing engages a player, attach the top 2 cards of that player's
-- deck to it.
instance HasAbilities NamelessThing where
  getAbilities (NamelessThing a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage NamelessThing where
  runMessage msg e@(NamelessThing attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> attachFromTopOfDeck pid 2 attrs
      pure e
    _ -> NamelessThing <$> runMessage msg attrs
