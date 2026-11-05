module LordOfTheRings.Enemy.Cards.PaleGreenTentacle (paleGreenTentacle) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype PaleGreenTentacle = PaleGreenTentacle EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

paleGreenTentacle :: EnemyCard PaleGreenTentacle
paleGreenTentacle = enemy PaleGreenTentacle Cards.paleGreenTentacle

instance HasModifiersFor PaleGreenTentacle where
  getModifiersFor (PaleGreenTentacle a) = do
    modifySelf a [CannotHaveAttachments]
    watchers <- select (EnemyWithCardCode $ toCardCode Cards.watcherInTheWater)
    for_ watchers $ \eid -> modified a eid [CannotTakeDamage]

instance HasAbilities PaleGreenTentacle where
  getAbilities (PaleGreenTentacle a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage PaleGreenTentacle where
  runMessage msg e@(PaleGreenTentacle a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      whenM (attackDestroyedACharacter a.id) $ placeEnemy a.id StagingArea
      pure e
    _ -> PaleGreenTentacle <$> runMessage msg a
