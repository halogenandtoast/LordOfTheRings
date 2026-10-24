module LordOfTheRings.Attachment.Cards.DarkKnowledge (darkKnowledge) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DarkKnowledge = DarkKnowledge AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

darkKnowledge :: AttachmentCard DarkKnowledge
darkKnowledge = attachment DarkKnowledge Cards.darkKnowledge

instance HasModifiersFor DarkKnowledge where
  getModifiersFor (DarkKnowledge a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier (-1)]
    _ -> pure ()

-- | Response: Exhaust Dark Knowledge to look at 1 shadow card that was just dealt to an
-- enemy attacking you.
--
-- Shadow cards are dealt before the attacks, so "attacking you" reads as engaged with you.
instance HasAbilities DarkKnowledge where
  getAbilities (DarkKnowledge a) =
    [mkAbility a 1 $ ResponseAbility (ShadowCardDealtWindow After a.controller) (ExhaustCost $ toTarget a)]

instance RunMessage DarkKnowledge where
  runMessage msg d@(DarkKnowledge attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 windows -> do
      for_ (headMay [(eid, cid) | Window _ (ShadowCardDealt eid cid _) <- windows]) \(eid, cid) ->
        lookAtShadowCard pid attrs eid cid
      pure d
    _ -> DarkKnowledge <$> runMessage msg attrs
