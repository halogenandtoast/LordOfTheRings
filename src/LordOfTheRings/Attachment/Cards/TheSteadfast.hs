module LordOfTheRings.Attachment.Cards.TheSteadfast (theSteadfast) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype TheSteadfast = TheSteadfast AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Accolade boon of the Ered Mithrin campaign. Permanent. Setup: Attach to a hero.
theSteadfast :: AttachmentCard TheSteadfast
theSteadfast = attachment TheSteadfast Cards.theSteadfast

-- | Attached hero gets +1 [willpower].
instance HasModifiersFor TheSteadfast where
  getModifiersFor (TheSteadfast a) = withHost a \host -> modified a host [WillpowerModifier 1]

-- | Response: After attached hero commits to the quest, if your threat is 40 or higher,
-- place 1 progress on the current quest.
instance HasAbilities TheSteadfast where
  getAbilities (TheSteadfast a) =
    [ restricted a 1 (PlayerExists $ ActivePlayer <> PlayerWithThreatAtLeast 40) $ response $ CommittedToQuestWindow After host
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage TheSteadfast where
  runMessage msg s@(TheSteadfast a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgressOnCurrentQuest a 1
      pure s
    _ -> TheSteadfast <$> runMessage msg a
