module LordOfTheRings.Attachment.Cards.ProtectorOfLorien (protectorOfLorien) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ProtectorOfLorien = ProtectorOfLorien AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

protectorOfLorien :: AttachmentCard ProtectorOfLorien
protectorOfLorien = attachment ProtectorOfLorien Cards.protectorOfLorien

-- | Action: Discard a card from your hand to give attached hero +1 defense or +1
-- willpower until the end of the phase. Limit 3 times per phase.
instance HasAbilities ProtectorOfLorien where
  getAbilities (ProtectorOfLorien a) =
    [ limited (GroupLimit PerPhase 3) $ mkAbility a 1 $ ActionAbility (DiscardFromHandCost a.owner 1)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage ProtectorOfLorien where
  runMessage msg s@(ProtectorOfLorien a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      case a.placement of
        AttachedTo host -> do
          let boost m = CreateEffect $ Effect (toSource a) host [m] UntilEndOfPhase
          push
            $ questionLabelWith "Give the attached hero" (Just $ toTarget a) [host] pid
              [Label "+1 defense" [boost $ DefenseModifier 1], Label "+1 willpower" [boost $ WillpowerModifier 1]]
        _ -> pure ()
      pure s
    _ -> ProtectorOfLorien <$> runMessage msg a
