module LordOfTheRings.Enemy.Cards.HuornOfFangorn (huornOfFangorn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype HuornOfFangorn = HuornOfFangorn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

huornOfFangorn :: EnemyCard HuornOfFangorn
huornOfFangorn = enemy HuornOfFangorn Cards.huornOfFangorn

-- | Hinder. Cannot have attachments.
instance HasModifiersFor HuornOfFangorn where
  getModifiersFor (HuornOfFangorn a) = modified a a [CannotHaveAttachments]

-- | Forced: At the beginning of each resource phase, the engaged player discards 1
-- attachment he controls.
instance HasAbilities HuornOfFangorn where
  getAbilities (HuornOfFangorn a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After ResourcePhase | EngagedWith _ <- [a.placement]]

-- | Shadow: Deal 1 damage to the defending character.
instance RunMessage HuornOfFangorn where
  runMessage msg e@(HuornOfFangorn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard attrs pid attachments
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure e
    _ -> HuornOfFangorn <$> runMessage msg attrs
