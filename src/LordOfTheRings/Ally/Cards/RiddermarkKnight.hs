module LordOfTheRings.Ally.Cards.RiddermarkKnight (riddermarkKnight) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype RiddermarkKnight = RiddermarkKnight AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

riddermarkKnight :: AllyCard RiddermarkKnight
riddermarkKnight = ally RiddermarkKnight Cards.riddermarkKnight

-- | Response: After Riddermark Knight is declared as an attacker, it gets +2 attack for
-- this attack. At the end of this attack, discard Riddermark Knight.
instance HasAbilities RiddermarkKnight where
  getAbilities (RiddermarkKnight a) = [mkAbility a 1 $ response $ AttackerDeclaredWindow After (Just $ toTarget a)]

instance RunMessage RiddermarkKnight where
  runMessage msg (RiddermarkKnight a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      phaseModifiers a a [AttackModifier 2, DiscardAfterAttack]
      pure $ RiddermarkKnight a
    StopAttacking (isTarget a -> True) -> do
      leaves <- elem DiscardAfterAttack <$> getModifiers a
      when leaves $ discard a
      RiddermarkKnight <$> runMessage msg a
    _ -> RiddermarkKnight <$> runMessage msg a
