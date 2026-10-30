module LordOfTheRings.Attachment.Cards.Windfola (windfola) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype Windfola = Windfola AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a spirit hero, or to Éowyn. Restricted, on its card def.
windfola :: AttachmentCard Windfola
windfola =
  attachment Windfola Cards.windfola
    & attachTo (AttachToCharacterMatching (HeroMatchAny [HeroWithSphere Spirit, HeroWithTitle "Éowyn"]) (AllyWithTitle "Éowyn"))

-- | Attached character gets +1 willpower.
instance HasModifiersFor Windfola where
  getModifiersFor (Windfola a) = withHost a \host -> modified a host [WillpowerModifier 1]

-- | Response: After attached character is removed from the quest, exhaust Windfola to
-- commit that character to the quest.
instance HasAbilities Windfola where
  getAbilities (Windfola a) =
    [mkAbility a 1 $ ResponseAbility (RemovedFromQuestWindow After host) (exhaustCost a) | AttachedTo host <- [a.placement]]

instance RunMessage Windfola where
  runMessage msg s@(Windfola a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a commitToQuest
      pure s
    _ -> Windfola <$> runMessage msg a
