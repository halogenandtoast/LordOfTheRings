module LordOfTheRings.Enemy.Cards.EastBightPatrol (eastBightPatrol) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EastBightPatrol = EastBightPatrol EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eastBightPatrol :: EnemyCard EastBightPatrol
eastBightPatrol = enemy EastBightPatrol Cards.eastBightPatrol

-- | Shadow: attacking enemy gets +1 attack. (If this attack is undefended, also raise your
-- threat by 3.)
instance RunMessage EastBightPatrol where
  runMessage msg e@(EastBightPatrol attrs) = case msg of
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      pushAll
        $ CreateEffect (Effect (toSource attrs) (toTarget eid) [AttackModifier 1] UntilEndOfPhase)
        : [RaiseThreat pid 3 | isNothing defender]
      pure e
    _ -> EastBightPatrol <$> runMessage msg attrs
