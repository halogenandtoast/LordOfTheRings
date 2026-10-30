module LordOfTheRings.Attachment.Cards.SwordThain (swordThain) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype SwordThain = SwordThain AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a unique ally belonging to any sphere of influence (neutral is none).
swordThain :: AttachmentCard SwordThain
swordThain =
  attachment SwordThain Cards.swordThain
    & attachTo (AttachToAlly $ UniqueAlly <> NotAlly (AllyWithSphere Neutral))

-- | Attached character loses the ally card type and gains the hero card type. Once
-- Sword-thain leaves it, it is an ally again.
instance RunMessage SwordThain where
  runMessage msg (SwordThain a) = case msg of
    AttachCard aid (AllyTarget ally) | aid == a.id -> do
      a' <- runMessage msg a
      push $ AllyBecomesHero ally
      pure $ SwordThain a'
    Discard (isTarget a -> True) -> do
      withHost a \case
        HeroTarget hid -> push $ HeroBecomesAlly hid
        _ -> pure ()
      SwordThain <$> runMessage msg a
    _ -> SwordThain <$> runMessage msg a
