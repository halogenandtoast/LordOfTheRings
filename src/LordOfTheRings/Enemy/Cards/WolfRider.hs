module LordOfTheRings.Enemy.Cards.WolfRider (wolfRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WolfRider = WolfRider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolfRider :: EnemyCard WolfRider
wolfRider = enemy WolfRider Cards.wolfRider

-- | Surge. Shadow: Wolf Rider attacks the defending player. That player may declare 1
-- character as a defender. Deal Wolf Rider its own Shadow card. After combat, return Wolf
-- Rider to the top of the encounter deck.
instance RunMessage WolfRider where
  runMessage msg e@(WolfRider attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      push $ PutShadowCardIntoPlay eid cid pid
      pure e
    _ -> WolfRider <$> runMessage msg attrs
