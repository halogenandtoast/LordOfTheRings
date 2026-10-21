module LordOfTheRings.Attachment.Cards.SupportOfTheEagles (supportOfTheEagles) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Trait (Trait (Eagle))

newtype SupportOfTheEagles = SupportOfTheEagles AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Tactics hero.
supportOfTheEagles :: AttachmentCard SupportOfTheEagles
supportOfTheEagles = attachment SupportOfTheEagles Cards.supportOfTheEagles & attachTo (AttachToHero $ HeroWithSphere Tactics)

-- | Action: Exhaust Support of the Eagles to choose an Eagle ally. Until the end of the
-- phase, attached hero adds that ally's attack or defense (choose 1) to its own.
instance HasAbilities SupportOfTheEagles where
  getAbilities (SupportOfTheEagles a) =
    [restricted a 1 (exists $ AllyWithTrait Eagle) $ ActionAbility (exhaustCost a) | AttachedTo _ <- [a.placement]]

instance RunMessage SupportOfTheEagles where
  runMessage msg s@(SupportOfTheEagles a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      eagles <- select (AllyWithTrait Eagle)
      withHost a \host -> chooseOneM pid do
        questionLabeled "supportOfTheEagles.chooseEagle"
        causedBy a
        targets eagles \e -> do
          attack <- getAttack (toTarget e)
          defense <- getDefense (toTarget e)
          chooseOneM pid do
            questionLabeled "supportOfTheEagles.chooseStat"
            causedBy a
            labeledWith "supportOfTheEagles.attack" [intVar "n" attack] $ phaseModifier a host (AttackModifier attack)
            labeledWith "supportOfTheEagles.defense" [intVar "n" defense] $ phaseModifier a host (DefenseModifier defense)
      pure s
    _ -> SupportOfTheEagles <$> runMessage msg a
