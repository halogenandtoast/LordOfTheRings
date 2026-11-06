module LordOfTheRings.Attachment.Cards.TheSecretOfLeadership (theSecretOfLeadership) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), CampaignLogUpdate (AddToCount))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype TheSecretOfLeadership = TheSecretOfLeadership AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

theSecretOfLeadership :: AttachmentCard TheSecretOfLeadership
theSecretOfLeadership = attachment TheSecretOfLeadership Cards.theSecretOfLeadership & attachTo (AttachToHero AnyHero)

instance HasModifiersFor TheSecretOfLeadership where
  getModifiersFor (TheSecretOfLeadership a) = withHost a $ \host -> modified a host [GainResourceIcon Leadership]

-- The campaign log keeps the card-wide use across games and changes of controller.
instance HasAbilities TheSecretOfLeadership where
  getAbilities (TheSecretOfLeadership a) =
    [ restricted
        a
        1
        ( PlayerExists
            ( PlayerWithId a.controller
                <> PlayerWithCampaignCardUsesBelow (toCardCode Cards.theSecretOfLeadership) 1
            )
            <> HeroExists ExhaustedHero
        )
        $ ActionAbility (ResourceCost host 1)
    | AttachedTo host@(HeroTarget _) <- [a.placement]
    ]

instance RunMessage TheSecretOfLeadership where
  runMessage msg s@(TheSecretOfLeadership a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      updateCampaignLog (AddToCount CardUses (toCardCode Cards.theSecretOfLeadership) 1)
      heroes <- select ExhaustedHero
      chooseOrRunOneM pid do
        questionLabeled "campaign.adaptationTarget"
        targets heroes ready
      pure s
    _ -> TheSecretOfLeadership <$> runMessage msg a
