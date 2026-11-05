module LordOfTheRings.Attachment.Cards.Athelas (athelas) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Condition, Dunedain, Healer))

newtype Athelas = Athelas AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dúnedain or Healer character.
athelas :: AttachmentCard Athelas
athelas =
  attachment Athelas Cards.athelas
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Dunedain, HeroWithTrait Healer])
          (AllyMatchAny [AllyWithTrait Dunedain, AllyWithTrait Healer])
      )

-- | Action: Discard Athelas and exhaust attached character to heal all damage on a
-- character. You may discard a Condition attachment from that character.
instance HasAbilities Athelas where
  getAbilities (Athelas a) =
    [mkAbility a 1 $ ActionAbility (ExhaustCost host) | AttachedTo host <- [a.placement]]

instance RunMessage Athelas where
  runMessage msg s@(Athelas a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a a
      characters <- selectCharacters AnyHero (AllyControlledBy AnyPlayer)
      chooseOneM pid do
        questionLabeled "athelas.heal"
        causedBy a
        targets characters \c -> do
          healAllDamage a c
          conditions <- attachedWithTrait Condition c
          unless (null conditions) $ chooseOneM pid do
            questionLabeled "athelas.discardCondition"
            causedBy a
            targets conditions (discard a)
            labeled "athelas.keep" (pure ())
      pure s
    _ -> Athelas <$> runMessage msg a
