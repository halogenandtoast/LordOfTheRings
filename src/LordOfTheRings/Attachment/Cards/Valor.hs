module LordOfTheRings.Attachment.Cards.Valor (valor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Valor = Valor AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Revised Core campaign. Permanent. Limit 1 per hero.
valor :: AttachmentCard Valor
valor = attachment Valor Cards.valor

-- | Response: After attached hero is declared as an attacker, exhaust Valor to heal 1
-- damage from attached hero and deal 1 damage to the defending enemy.
instance HasAbilities Valor where
  getAbilities (Valor a) =
    [mkAbility a 1 $ ResponseAbility (AttackerDeclaredWindow After (Just host)) (exhaustCost a) | AttachedTo host <- [a.placement]]

instance RunMessage Valor where
  runMessage msg s@(Valor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      withHost a \host -> healDamage a host 1
      for_ [eid | Window _ (DeclaredAsAttacker _ eid) <- windows] \eid -> dealDamage a eid 1
      pure s
    _ -> Valor <$> runMessage msg a
