module LordOfTheRings.Attachment.Cards.SpearOfTheCitadel (spearOfTheCitadel) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype SpearOfTheCitadel = SpearOfTheCitadel AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Restricted. Attach to a tactics character. Limit 1 per character.
spearOfTheCitadel :: AttachmentCard SpearOfTheCitadel
spearOfTheCitadel =
  attachment SpearOfTheCitadel Cards.spearOfTheCitadel
    & attachTo (AttachToCharacterMatching (HeroWithSphere Tactics) (AllyWithSphere Tactics))

-- | Response: After attached character is declared as a defender, deal 1 damage to the
-- attacking enemy.
instance HasAbilities SpearOfTheCitadel where
  getAbilities (SpearOfTheCitadel a) =
    [mkAbility a 1 $ response $ DefenderDeclaredWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage SpearOfTheCitadel where
  runMessage msg s@(SpearOfTheCitadel a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (headMay [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> dealDamage a eid 1
      pure s
    _ -> SpearOfTheCitadel <$> runMessage msg a
