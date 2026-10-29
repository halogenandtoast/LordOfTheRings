module LordOfTheRings.Enemy.Cards.GiantSwampAdder (giantSwampAdder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype GiantSwampAdder = GiantSwampAdder EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantSwampAdder :: EnemyCard GiantSwampAdder
giantSwampAdder = enemy GiantSwampAdder Cards.giantSwampAdder

-- | Cannot have attachments.
instance HasModifiersFor GiantSwampAdder where
  getModifiersFor (GiantSwampAdder a) = modified a a [CannotHaveAttachments]

-- | Forced: After any number of time counters are removed from the current quest, Giant
-- Swamp Adder attacks the engaged player.
instance HasAbilities GiantSwampAdder where
  getAbilities (GiantSwampAdder a) =
    [ restricted a 1 (exists $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer)
        $ forced
        $ TimeCountersRemovedFromQuestWindow After
    ]

instance RunMessage GiantSwampAdder where
  runMessage msg e@(GiantSwampAdder attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (placementController attrs.placement) \pid -> immediateAttack pid attrs.id
      pure e
    _ -> GiantSwampAdder <$> runMessage msg attrs
