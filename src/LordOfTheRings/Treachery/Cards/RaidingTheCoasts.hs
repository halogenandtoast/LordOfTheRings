module LordOfTheRings.Treachery.Cards.RaidingTheCoasts (raidingTheCoasts) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RaidingTheCoasts = RaidingTheCoasts TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

raidingTheCoasts :: TreacheryCard RaidingTheCoasts
raidingTheCoasts = treachery RaidingTheCoasts Cards.raidingTheCoasts

-- | When Revealed: Each player must choose: either add 3 to the total threat in the
-- staging area until the end of the phase, or discard the highest cost attachment you
-- control. The discard is offered only to a player with an attachment.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, discard an
-- attachment you control.
instance RunMessage RaidingTheCoasts where
  runMessage msg t@(RaidingTheCoasts attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        attachments <- filter (\case AttachmentTarget _ -> True; _ -> False) <$> getAttachmentsControlledBy p
        chooseOrRunOneM p do
          questionLabeled "raidingTheCoasts.choose"
          causedBy attrs
          labeled "raidingTheCoasts.threat" $ phaseModifier attrs GameTarget (StagingThreatBonus 3)
          unless (null attachments) $ labeled "raidingTheCoasts.discard" $ discardHighestCostAttachment attrs p
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure t
    _ -> RaidingTheCoasts <$> runMessage msg attrs
