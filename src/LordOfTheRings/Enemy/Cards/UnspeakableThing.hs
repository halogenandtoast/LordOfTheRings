module LordOfTheRings.Enemy.Cards.UnspeakableThing (unspeakableThing) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (attachFromTopOfDeck, namelessX)

newtype UnspeakableThing = UnspeakableThing EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

unspeakableThing :: EnemyCard UnspeakableThing
unspeakableThing = enemy UnspeakableThing Cards.unspeakableThing

-- | X is the printed cost of all attached cards on this card. If there are no cards
-- attached, X is 5.
instance HasModifiersFor UnspeakableThing where
  getModifiersFor (UnspeakableThing a) = let x = namelessX 5 a in modified a a [AttackModifier x, HitPointModifier x]

-- | Forced: When Unspeakable Thing engages a player or attacks a player, attach the top
-- card of that player's deck to it.
instance HasAbilities UnspeakableThing where
  getAbilities (UnspeakableThing a) =
    [ mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id
    , mkAbility a 2 $ forced $ EnemyAttacksWindow When a.id
    ]

instance RunMessage UnspeakableThing where
  runMessage msg e@(UnspeakableThing attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> attachFromTopOfDeck pid 1 attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> attachFromTopOfDeck pid 1 attrs
      pure e
    _ -> UnspeakableThing <$> runMessage msg attrs
