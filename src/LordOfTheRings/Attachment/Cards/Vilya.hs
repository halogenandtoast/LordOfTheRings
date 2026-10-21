module LordOfTheRings.Attachment.Cards.Vilya (vilya) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype Vilya = Vilya AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

vilya :: AttachmentCard Vilya
vilya = attachment Vilya Cards.vilya & attachTo (AttachToHero $ HeroWithTitle "Elrond")

-- | Attach to Elrond. He gains a spirit resource icon.
instance HasModifiersFor Vilya where
  getModifiersFor (Vilya a) = withHost a \host -> modified a host [GainResourceIcon Spirit]

-- | Action: Exhaust Elrond and Vilya to reveal the top card of your deck. You can
-- immediately play or put into play the revealed card for no cost, if able. Otherwise,
-- move the revealed card to the bottom of your deck.
instance HasAbilities Vilya where
  getAbilities (Vilya a) =
    [mkAbility a 1 $ ActionAbility (Costs [ExhaustCost (toTarget a), ExhaustCost host]) | AttachedTo host <- [a.placement]]

instance RunMessage Vilya where
  runMessage msg s@(Vilya a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      push $ PlayTopOfDeckAtNoCostOrBottom pid (toSource a)
      pure s
    _ -> Vilya <$> runMessage msg a
