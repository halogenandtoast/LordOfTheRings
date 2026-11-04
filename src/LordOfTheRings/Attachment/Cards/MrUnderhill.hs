module LordOfTheRings.Attachment.Cards.MrUnderhill (mrUnderhill) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (RingBearer))

newtype MrUnderhill = MrUnderhill AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Black Riders. Permanent, on its card def. Setup: Attach to the
-- Ring-bearer: as the Campaign Pool records it on its hero.
mrUnderhill :: AttachmentCard MrUnderhill
mrUnderhill = attachment MrUnderhill Cards.mrUnderhill & attachTo (AttachToHero $ HeroWithTrait RingBearer)

-- | Action: Add Mr. Underhill to the victory display to choose an enemy engaged with you.
-- Until the end of the round, that enemy cannot attack you.
instance HasAbilities MrUnderhill where
  getAbilities (MrUnderhill a) = [restricted a 1 (exists $ EnemyEngagedWith $ PlayerWithId a.controller) $ ActionAbility Free]

instance RunMessage MrUnderhill where
  runMessage msg s@(MrUnderhill a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      addToVictoryDisplay a
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "mrUnderhill.enemy"
        causedBy a
        targets enemies \eid -> roundModifier a eid (CannotAttackPlayer pid)
      pure s
    _ -> MrUnderhill <$> runMessage msg a
