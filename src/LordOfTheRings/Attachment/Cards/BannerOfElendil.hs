module LordOfTheRings.Attachment.Cards.BannerOfElendil (bannerOfElendil) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype BannerOfElendil = BannerOfElendil AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Aragorn.
bannerOfElendil :: AttachmentCard BannerOfElendil
bannerOfElendil = attachment BannerOfElendil Cards.bannerOfElendil & attachTo (AttachToHero $ HeroWithTitle "Aragorn")

-- | While Aragorn is committed to the quest, each other questing character you control
-- gets +1 willpower. While Aragorn is attacking, each other attacking character you
-- control gets +1 attack.
instance HasModifiersFor BannerOfElendil where
  getModifiersFor (BannerOfElendil a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      let you = PlayerWithId a.controller
          others hm am = filter (/= host) <$> selectCharacters (HeroControlledBy you <> hm) (AllyControlledBy you <> am)
      questing <- selectAny (HeroWithId hid <> CommittedHero)
      when questing $ traverse_ (\c -> modified a c [WillpowerModifier 1]) =<< others CommittedHero CommittedAlly
      attacking <- selectAny (HeroWithId hid <> AttackingHero)
      when attacking $ traverse_ (\c -> modified a c [AttackModifier 1]) =<< others AttackingHero AttackingAlly
    _ -> pure ()
