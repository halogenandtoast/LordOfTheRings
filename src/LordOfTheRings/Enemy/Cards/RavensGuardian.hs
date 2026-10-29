module LordOfTheRings.Enemy.Cards.RavensGuardian (ravensGuardian) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype RavensGuardian = RavensGuardian EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravensGuardian :: EnemyCard RavensGuardian
ravensGuardian = enemy RavensGuardian Cards.ravensGuardian

-- | Cannot have non-Key attachments: a Key comes as an objective, which this does not bar.
instance HasModifiersFor RavensGuardian where
  getModifiersFor (RavensGuardian a) = modified a a [CannotHaveAttachments]

-- | Forced: After the last time counter is removed from Raven's Guardian deal 1 damage to
-- each character controlled by the engaged player. Place 4 time counters on Raven's
-- Guardian.
instance HasAbilities RavensGuardian where
  getAbilities (RavensGuardian a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

instance RunMessage RavensGuardian where
  runMessage msg e@(RavensGuardian attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (placementController attrs.placement) \pid -> do
        characters <- charactersControlledBy pid
        for_ characters \c -> dealDamage attrs c 1
      placeTimeCounters attrs attrs 4
      pure e
    _ -> RavensGuardian <$> runMessage msg attrs
