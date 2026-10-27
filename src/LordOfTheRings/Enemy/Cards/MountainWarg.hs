module LordOfTheRings.Enemy.Cards.MountainWarg (mountainWarg) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Mountain))

newtype MountainWarg = MountainWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainWarg :: EnemyCard MountainWarg
mountainWarg = enemy MountainWarg Cards.mountainWarg

-- | If Mountain Warg is dealt a shadow card with no effect, return Mountain Warg to the
-- staging area after it attacks.
-- Shadow: attacking enemy gets +1 [Attack] (+2 [Attack] instead if a Mountain is the
-- active location.)
instance RunMessage MountainWarg where
  runMessage msg e@(MountainWarg attrs) = case msg of
    ResolveShadow eid _ _ | eid == attrs.id -> do
      let faceDown = filter ((`notElem` attrs.enemyShadowRevealed) . (.id)) attrs.shadowCards
      when (any (not . cdShadowEffect . toCardDef) faceDown) $ returnToStagingAfterAttack eid
      MountainWarg <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      mountain <- selectAny (IsActiveLocation <> LocationWithTrait Mountain)
      enemyGetsAttack attrs eid (if mountain then 2 else 1)
      pure e
    _ -> MountainWarg <$> runMessage msg attrs
