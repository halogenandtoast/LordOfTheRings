module LordOfTheRings.Ally.Cards.WellinghallPreserver (wellinghallPreserver) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Ent))

newtype WellinghallPreserver = WellinghallPreserver AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Enters play exhausted.
wellinghallPreserver :: AllyCard WellinghallPreserver
wellinghallPreserver = ally (WellinghallPreserver . (exhaustedL .~ True)) Cards.wellinghallPreserver

-- | Cannot have restricted attachments.
instance HasModifiersFor WellinghallPreserver where
  getModifiersFor (WellinghallPreserver a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Response: After Wellinghall Preserver readies, heal 1 damage from an Ent character.
instance HasAbilities WellinghallPreserver where
  getAbilities (WellinghallPreserver a) = [mkAbility a 1 $ response $ BecameReadyWindow After (toTarget a)]

instance RunMessage WellinghallPreserver where
  runMessage msg s@(WellinghallPreserver a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      damaged <- filterM (fmap (> 0) . getDamage) =<< selectCharacters (HeroWithTrait Ent) (AllyWithTrait Ent)
      chooseOrRunOneM pid do
        questionLabeled "chooseCharacter"
        causedBy a
        targets damaged \c -> healDamage a c 1
      pure s
    _ -> WellinghallPreserver <$> runMessage msg a
