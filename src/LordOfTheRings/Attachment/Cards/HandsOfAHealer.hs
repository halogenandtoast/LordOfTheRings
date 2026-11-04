module LordOfTheRings.Attachment.Cards.HandsOfAHealer (handsOfAHealer) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Healer))

newtype HandsOfAHealer = HandsOfAHealer AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Treason of Saruman. Permanent, on its card def. Setup: Attach to a Healer
-- hero: as the Campaign Pool records it on its hero.
handsOfAHealer :: AttachmentCard HandsOfAHealer
handsOfAHealer = attachment HandsOfAHealer Cards.handsOfAHealer & attachTo (AttachToHero $ HeroWithTrait Healer)

-- | Action: Exhaust attached hero and Hands of a Healer to heal up to 3 damage from a hero.
instance HasAbilities HandsOfAHealer where
  getAbilities (HandsOfAHealer a) =
    [mkAbility a 1 $ ActionAbility (Costs [ExhaustCost host, exhaustCost a]) | AttachedTo host <- [a.placement]]

instance RunMessage HandsOfAHealer where
  runMessage msg s@(HandsOfAHealer a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      chooseOneM pid do
        questionLabeled "chooseHero"
        causedBy a
        targets heroes \h -> healDamage a h 3
      pure s
    _ -> HandsOfAHealer <$> runMessage msg a
