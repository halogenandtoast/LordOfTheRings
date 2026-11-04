module LordOfTheRings.Enemy.Cards.BlackRider (blackRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Morgul))

newtype BlackRider = BlackRider EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Hide 2 (the game makes the test as it is revealed).
blackRider :: EnemyCard BlackRider
blackRider = enemy BlackRider Cards.blackRider

-- | Cannot have non-Morgul attachments.
instance HasModifiersFor BlackRider where
  getModifiersFor (BlackRider a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]

-- | Forced: After engaged player fails a Hide test, Black Rider makes an immediate attack.
instance HasAbilities BlackRider where
  getAbilities (BlackRider a) = [mkAbility a 1 $ forced $ FailedHideTestWindow After (PlayerEngagedWith $ EnemyWithId a.id)]

instance RunMessage BlackRider where
  runMessage msg e@(BlackRider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (FailedHideTest pid) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    _ -> BlackRider <$> runMessage msg attrs
