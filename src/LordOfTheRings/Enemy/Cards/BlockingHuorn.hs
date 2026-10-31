module LordOfTheRings.Enemy.Cards.BlockingHuorn (blockingHuorn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype BlockingHuorn = BlockingHuorn EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

blockingHuorn :: EnemyCard BlockingHuorn
blockingHuorn = enemy BlockingHuorn Cards.blockingHuorn

-- | Hinder. Cannot have attachments. While Blocking Huorn is engaged with a player, it adds
-- its Threat to the staging area.
instance HasModifiersFor BlockingHuorn where
  getModifiersFor (BlockingHuorn a) = do
    modified a a [CannotHaveAttachments]
    case a.placement of
      EngagedWith _ -> modified a GameTarget [EngagedEnemiesAddThreat (EnemyWithId a.id)]
      _ -> pure ()

-- | Shadow: Put Blocking Huorn into play engaged with you.
instance RunMessage BlockingHuorn where
  runMessage msg e@(BlockingHuorn attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      putShadowEnemyIntoPlay eid cid pid
      pure e
    _ -> BlockingHuorn <$> runMessage msg attrs
