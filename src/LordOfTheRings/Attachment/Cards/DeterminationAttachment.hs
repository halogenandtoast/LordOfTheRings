module LordOfTheRings.Attachment.Cards.DeterminationAttachment (determinationAttachment) where

import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Campaign (CampaignLogKey (Determined))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Trait (Trait (Condition))

newtype DeterminationAttachment = DeterminationAttachment AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Determination, the event, once played and attached to its unique character.
determinationAttachment :: AttachmentCard DeterminationAttachment
determinationAttachment = attachment DeterminationAttachment Events.determination

-- | Counts as a Condition attachment with the text: "Forced: If you win this scenario,
-- mark in the Campaign Log that attached character is 'determined'."
instance HasModifiersFor DeterminationAttachment where
  getModifiersFor (DeterminationAttachment a) = do
    modifySelf a [GainTrait Condition]
    for_ [h | AttachedTo h <- [a.placement]] \host -> do
      code <- case host of
        HeroTarget hid -> Just . toCardCode <$> getHero hid
        AllyTarget aid -> Just . toCardCode <$> getAlly aid
        _ -> pure Nothing
      for_ code \c -> modified a GameTarget [RecordedIfWon Determined c]
