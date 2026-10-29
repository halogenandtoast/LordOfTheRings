module LordOfTheRings.Enemy.Cards.RockAdder (rockAdder) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gameEffects)

newtype RockAdder = RockAdder EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

rockAdder :: EnemyCard RockAdder
rockAdder = enemy RockAdder Cards.rockAdder

-- | Rock Adder cannot be attacked unless it has dealt at least 1 damage this round.
instance HasModifiersFor RockAdder where
  getModifiersFor (RockAdder a) = do
    dealt <- getsGame (any (\e -> effectTarget e == toTarget a && DealtDamageThisRound `elem` effectModifiers e) . gameEffects)
    unless dealt $ modified a a [CannotBeAttacked]

-- | Shadow: If this attack is undefended, the defending player must discard 1 character he
-- controls from play.
instance RunMessage RockAdder where
  runMessage msg e@(RockAdder attrs) = case msg of
    -- the damage is dealt, not canceled
    DoStep 1 (DealDamage (sourceEnemy -> Just eid) _ n) | eid == attrs.id, n > 0 -> do
      roundModifier attrs attrs DealtDamageThisRound
      RockAdder <$> runMessage msg attrs
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      when (null defenders) $ chooseAndDiscard attrs pid =<< charactersControlledBy pid
      pure e
    _ -> RockAdder <$> runMessage msg attrs
