module LordOfTheRings.Ally.Cards.DerndingleWarrior (derndingleWarrior) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DerndingleWarrior = DerndingleWarrior AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def. Enters play exhausted.
derndingleWarrior :: AllyCard DerndingleWarrior
derndingleWarrior = ally (DerndingleWarrior . (exhaustedL .~ True)) Cards.derndingleWarrior

-- | Cannot have restricted attachments.
instance HasModifiersFor DerndingleWarrior where
  getModifiersFor (DerndingleWarrior a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | While Derndingle Warrior is defending, it gains: "Action: Deal 1 damage to Derndingle
-- Warrior to give it +3 defense for this attack." (Limit once per attack.)
instance HasAbilities DerndingleWarrior where
  getAbilities (DerndingleWarrior a) =
    [limited (PlayerLimit PerAttack 1) $ restricted a 1 (exists $ AllyWithId a.id <> DefendingAlly) $ ActionAbility Free]

instance RunMessage DerndingleWarrior where
  runMessage msg s@(DerndingleWarrior a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      dealDamage a a 1
      attackModifier a a (DefenseModifier 3)
      pure s
    _ -> DerndingleWarrior <$> runMessage msg a
