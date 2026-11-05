module LordOfTheRings.Attachment.Cards.DragonSlayer (dragonSlayer) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DragonSlayer = DragonSlayer AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Accolade boon of the Ered Mithrin campaign. Permanent. Setup: Attach to a hero.
dragonSlayer :: AttachmentCard DragonSlayer
dragonSlayer = attachment DragonSlayer Cards.dragonSlayer

-- | Attached hero gets +1 [attack].
instance HasModifiersFor DragonSlayer where
  getModifiersFor (DragonSlayer a) = withHost a \host -> modified a host [AttackModifier 1]

-- | Response: After a non-unique enemy is added to the staging area, exhaust Dragon-slayer
-- to engage that enemy. It makes an immediate attack against you.
instance HasAbilities DragonSlayer where
  getAbilities (DragonSlayer a) =
    [mkAbility a 1 $ ResponseAbility (MatchingEnemyEntersStagingAreaWindow After (NotEnemy UniqueEnemy)) (exhaustCost a)]

instance RunMessage DragonSlayer where
  runMessage msg s@(DragonSlayer a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (EntersStagingArea (EnemyTarget eid)) <- windows]) \eid -> do
        engageEnemy pid eid
        immediateAttack pid eid
      pure s
    _ -> DragonSlayer <$> runMessage msg a
