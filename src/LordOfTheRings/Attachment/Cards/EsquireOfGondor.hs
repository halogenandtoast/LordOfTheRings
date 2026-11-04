module LordOfTheRings.Attachment.Cards.EsquireOfGondor (esquireOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype EsquireOfGondor = EsquireOfGondor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Permanent. Setup: Attach to a hero: the game attaches it as the scenario is set up
-- ('cdSetupAttach'), or to the hero the campaign log records it on.
esquireOfGondor :: AttachmentCard EsquireOfGondor
esquireOfGondor = attachment EsquireOfGondor Cards.esquireOfGondor & attachTo (AttachToHero AnyHero)

-- | Attached hero gains the Gondor trait. Attached hero gets +1 willpower while committed
-- to the quest with another Gondor character.
instance HasModifiersFor EsquireOfGondor where
  getModifiersFor (EsquireOfGondor a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      committed <- selectAny (HeroWithId hid <> CommittedHero)
      others <- filter (/= host) <$> selectCharacters (CommittedHero <> HeroWithTrait Gondor) (CommittedAlly <> AllyWithTrait Gondor)
      modified a host $ GainTrait Gondor : [WillpowerModifier 1 | committed, notNull others]
    _ -> pure ()
