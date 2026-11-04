module LordOfTheRings.Attachment.Cards.EsquireOfRohan (esquireOfRohan) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype EsquireOfRohan = EsquireOfRohan AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Permanent. Setup: Attach to a hero: the game attaches it as the scenario is set up
-- ('cdSetupAttach'), or to the hero the campaign log records it on.
esquireOfRohan :: AttachmentCard EsquireOfRohan
esquireOfRohan = attachment EsquireOfRohan Cards.esquireOfRohan & attachTo (AttachToHero AnyHero)

-- | Attached hero gains the Rohan trait. Attached hero gets +1 attack while attacking with
-- another Rohan character.
instance HasModifiersFor EsquireOfRohan where
  getModifiersFor (EsquireOfRohan a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      attacking <- selectAny (HeroWithId hid <> AttackingHero)
      others <- filter (/= host) <$> selectCharacters (AttackingHero <> HeroWithTrait Rohan) (AttackingAlly <> AllyWithTrait Rohan)
      modified a host $ GainTrait Rohan : [AttackModifier 1 | attacking, notNull others]
    _ -> pure ()
