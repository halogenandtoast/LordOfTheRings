module LordOfTheRings.Enemy.Cards.DunlendingBandit (dunlendingBandit) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype DunlendingBandit = DunlendingBandit EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlendingBandit :: EnemyCard DunlendingBandit
dunlendingBandit = enemy DunlendingBandit Cards.dunlendingBandit

handSize :: HasGame m => PlayerId -> m Int
handSize = fmap (length . (.hand)) . getPlayer

-- | While engaged with a player, Dunlending Bandit gets +1 attack for each card in that
-- player's hand.
instance HasModifiersFor DunlendingBandit where
  getModifiersFor (DunlendingBandit a) = case a.placement of
    EngagedWith pid -> do
      n <- handSize pid
      when (n > 0) $ modified a a [AttackModifier n]
    _ -> pure ()

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player has at
-- least 3 cards in his hand).
instance RunMessage DunlendingBandit where
  runMessage msg e@(DunlendingBandit attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      n <- handSize pid
      enemyGetsAttack attrs eid (if n >= 3 then 2 else 1)
      pure e
    _ -> DunlendingBandit <$> runMessage msg attrs
