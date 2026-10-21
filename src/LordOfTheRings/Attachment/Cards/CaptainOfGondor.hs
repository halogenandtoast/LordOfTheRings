module LordOfTheRings.Attachment.Cards.CaptainOfGondor (captainOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Warrior))

newtype CaptainOfGondor = CaptainOfGondor AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

captainOfGondor :: AttachmentCard CaptainOfGondor
captainOfGondor = attachment CaptainOfGondor Cards.captainOfGondor & attachTo (AttachToHero $ HeroWithTrait Warrior)

-- | Response: After you optionally engage an enemy, exhaust Captain of Gondor to give
-- attached hero +1 attack and +1 defense until the end of the round.
instance HasAbilities CaptainOfGondor where
  getAbilities (CaptainOfGondor a) =
    [mkAbility a 1 $ ResponseAbility (OptionallyEngagedWindow After a.owner) (ExhaustCost $ toTarget a) | AttachedTo _ <- [a.placement]]

instance RunMessage CaptainOfGondor where
  runMessage msg c@(CaptainOfGondor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \host -> roundModifiers attrs host [AttackModifier 1, DefenseModifier 1]
      pure c
    _ -> CaptainOfGondor <$> runMessage msg attrs
