module LordOfTheRings.Attachment.Cards.SteedOfTheMark (steedOfTheMark) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor, Rohan))

newtype SteedOfTheMark = SteedOfTheMark AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

steedOfTheMark :: AttachmentCard SteedOfTheMark
steedOfTheMark =
  attachment SteedOfTheMark Cards.steedOfTheMark
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Gondor, HeroWithTrait Rohan])

-- | Response: after attached hero commits to a quest, spend 1 resource from attached
-- hero's resource pool to ready attached hero.
instance HasAbilities SteedOfTheMark where
  getAbilities (SteedOfTheMark a) =
    [mkAbility a 1 $ ResponseAbility (CommittedToQuestWindow After host) (ResourceCost host 1) | AttachedTo host <- [a.placement]]

instance RunMessage SteedOfTheMark where
  runMessage msg s@(SteedOfTheMark a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> SteedOfTheMark <$> runMessage msg a
