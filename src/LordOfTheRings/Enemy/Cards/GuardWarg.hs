module LordOfTheRings.Enemy.Cards.GuardWarg (guardWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype GuardWarg = GuardWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardWarg :: EnemyCard GuardWarg
guardWarg = enemy GuardWarg Cards.guardWarg

-- | Forced: After the engaged player discards any number of cards from his out-of-play
-- deck, Guard Warg makes an immediate attack. The scenario opens the window as he does.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage GuardWarg where
  runMessage msg e@(GuardWarg attrs) = case msg of
    CheckWindows ws
      | EngagedWith pid <- attrs.placement
      , pid `elem` [p | Window After (DiscardedFromOutOfPlayDeck p) <- ws] -> do
          immediateAttack pid attrs.id
          GuardWarg <$> runMessage msg attrs
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure e
    _ -> GuardWarg <$> runMessage msg attrs
