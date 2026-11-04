module LordOfTheRings.Attachment.Cards.LiveryOfTheTower (liveryOfTheTower) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype LiveryOfTheTower = LiveryOfTheTower AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Gondor hero. Restricted.
liveryOfTheTower :: AttachmentCard LiveryOfTheTower
liveryOfTheTower = attachment LiveryOfTheTower Cards.liveryOfTheTower & attachTo (AttachToHero $ HeroWithTrait Gondor)

-- | Response: Spend X resources from attached hero's pool to cancel X damage just dealt to
-- attached hero.
instance HasAbilities LiveryOfTheTower where
  getAbilities (LiveryOfTheTower a) =
    [ restricted a 1 (exists $ HeroWithId hid <> HeroWithResources) $ response $ dealtDamage (HeroTarget hid)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage LiveryOfTheTower where
  runMessage msg s@(LiveryOfTheTower a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> case a.placement of
      AttachedTo host@(HeroTarget hid) -> do
        resources <- (.resources) <$> getHero hid
        let dealt = sum [n | Window _ (DealtDamage t n) <- windows, t == host]
        chooseOneM pid do
          questionLabeled "liveryOfTheTower.chooseX"
          causedBy a
          for_ [1 .. min resources dealt] \x -> labeledWith "liveryOfTheTower.cancel" [intVar "count" x] do
            spendResourcesFrom pid host x
            removeTokens a host Damage x
        pure s
      _ -> pure s
    _ -> LiveryOfTheTower <$> runMessage msg a
