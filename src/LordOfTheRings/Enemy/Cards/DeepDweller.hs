module LordOfTheRings.Enemy.Cards.DeepDweller (deepDweller) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DeepDweller = DeepDweller EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepDweller :: EnemyCard DeepDweller
deepDweller = enemy DeepDweller Cards.deepDweller

-- | Forced: When Deep-dweller engages you, it makes an immediate attack.
-- Forced: After Deep-dweller takes any amount of damage (and survives), return it to the
-- staging area.
instance HasAbilities DeepDweller where
  getAbilities (DeepDweller a) =
    [ mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id
    , mkAbility a 2 $ forced $ DealtDamageWindow After (toTarget a)
    ]

instance RunMessage DeepDweller where
  runMessage msg e@(DeepDweller attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      hitPoints <- getEnemyHitPoints attrs.id
      when (attrs.damage < hitPoints && attrs.placement /= StagingArea) $ placeEnemy attrs.id StagingArea
      pure e
    _ -> DeepDweller <$> runMessage msg attrs
