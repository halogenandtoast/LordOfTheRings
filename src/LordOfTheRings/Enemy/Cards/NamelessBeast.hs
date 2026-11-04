module LordOfTheRings.Enemy.Cards.NamelessBeast (namelessBeast) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype NamelessBeast = NamelessBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

namelessBeast :: EnemyCard NamelessBeast
namelessBeast = enemy NamelessBeast Cards.namelessBeast

-- | Forced: When Nameless Beast attacks you, discard the top card of your deck. Nameless
-- Beast gets +X Attack for this attack, where X is that card's cost (its printed cost, 0
-- for one with none).
instance HasAbilities NamelessBeast where
  getAbilities (NamelessBeast a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Attacking enemy gets +X Attack, where X is the cost of the top card of your
-- discard pile.
instance RunMessage NamelessBeast where
  runMessage msg e@(NamelessBeast attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyAttacking _ pid) <- ws]) \pid -> do
        top <- headMay . (.deck) <$> getPlayer pid
        for_ top \c -> do
          discardTopOfDeck pid 1
          enemyGetsAttack attrs attrs.id (printedCostOf c)
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      top <- headMay . (.discard) <$> getPlayer pid
      for_ top \c -> enemyGetsAttack (CardIdSource cid) eid (printedCostOf c)
      pure e
    _ -> NamelessBeast <$> runMessage msg attrs
