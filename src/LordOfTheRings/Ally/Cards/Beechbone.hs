module LordOfTheRings.Ally.Cards.Beechbone (beechbone) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Beechbone = Beechbone AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Enters play exhausted.
beechbone :: AllyCard Beechbone
beechbone = ally (Beechbone . (exhaustedL .~ True)) Cards.beechbone

-- | Cannot have restricted attachments.
instance HasModifiersFor Beechbone where
  getModifiersFor (Beechbone a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Response: After Beechbone is declared as an attacker, deal 1 damage to him to deal X
-- damage to the defending enemy. X is the amount of damage on Beechbone.
instance HasAbilities Beechbone where
  getAbilities (Beechbone a) = [mkAbility a 1 $ response $ AttackerDeclaredWindow After (Just $ toTarget a)]

instance RunMessage Beechbone where
  runMessage msg s@(Beechbone a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      dealDamage a a 1
      -- X is read once the damage is on him
      doStep 1 msg
      pure s
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 windows) -> do
      x <- getDamage (toTarget a)
      for_ (take 1 [eid | Window _ (DeclaredAsAttacker _ eid) <- windows]) \eid -> dealDamage a eid x
      pure s
    _ -> Beechbone <$> runMessage msg a
