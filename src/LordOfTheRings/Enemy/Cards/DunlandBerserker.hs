module LordOfTheRings.Enemy.Cards.DunlandBerserker (dunlandBerserker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype DunlandBerserker = DunlandBerserker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlandBerserker :: EnemyCard DunlandBerserker
dunlandBerserker = enemy DunlandBerserker Cards.dunlandBerserker

-- | Forced: After the engaged player draws any number of cards, Dunland Berserker makes an
-- immediate attack.
instance HasAbilities DunlandBerserker where
  getAbilities (DunlandBerserker a) =
    [mkAbility a 1 $ forced $ DrewCardsWindow After (Just pid) | Just pid <- [placementController a.placement]]

-- | Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage DunlandBerserker where
  runMessage msg e@(DunlandBerserker attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | Just pid <- placementController attrs.placement -> do
      immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> DunlandBerserker <$> runMessage msg attrs
