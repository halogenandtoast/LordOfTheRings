module LordOfTheRings.Enemy.Cards.WolfsGuardian (wolfsGuardian) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype WolfsGuardian = WolfsGuardian EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolfsGuardian :: EnemyCard WolfsGuardian
wolfsGuardian = enemy WolfsGuardian Cards.wolfsGuardian

-- | Cannot have non-Key attachments: a Key comes as an objective, which this does not bar.
instance HasModifiersFor WolfsGuardian where
  getModifiersFor (WolfsGuardian a) = modified a a [CannotHaveAttachments]

-- | Forced: After the last time counter is removed from Wolf's Guardian it makes an
-- immediate attack against the engaged player. Place 3 time counters on Wolf's Guardian.
instance HasAbilities WolfsGuardian where
  getAbilities (WolfsGuardian a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

instance RunMessage WolfsGuardian where
  runMessage msg e@(WolfsGuardian attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (placementController attrs.placement) \pid -> immediateAttack pid attrs.id
      placeTimeCounters attrs attrs 3
      pure e
    _ -> WolfsGuardian <$> runMessage msg attrs
