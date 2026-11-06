module LordOfTheRings.Attachment.Cards.ThePowerOfSpirit (thePowerOfSpirit) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), CampaignLogUpdate (AddToCount))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype ThePowerOfSpirit = ThePowerOfSpirit AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

thePowerOfSpirit :: AttachmentCard ThePowerOfSpirit
thePowerOfSpirit = attachment ThePowerOfSpirit Cards.thePowerOfSpirit & attachTo (AttachToHero AnyHero)

instance HasModifiersFor ThePowerOfSpirit where
  getModifiersFor (ThePowerOfSpirit a) = withHost a $ \host -> modified a host [GainResourceIcon Spirit]

-- The campaign log keeps the card-wide use across games and changes of controller.
instance HasAbilities ThePowerOfSpirit where
  getAbilities (ThePowerOfSpirit a) =
    [ restricted
        a
        1
        ( PlayerExists
            (PlayerWithId a.controller <> PlayerWithCampaignCardUsesBelow (toCardCode Cards.thePowerOfSpirit) 1)
            <> LocationExists AnyLocation
        )
        $ ActionAbility (ExhaustCost host)
    | AttachedTo host@(HeroTarget _) <- [a.placement]
    ]

instance RunMessage ThePowerOfSpirit where
  runMessage msg s@(ThePowerOfSpirit a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      updateCampaignLog (AddToCount CardUses (toCardCode Cards.thePowerOfSpirit) 1)
      locations <- select AnyLocation
      chooseOrRunOneM pid do
        questionLabeled "campaign.adaptationTarget"
        targets locations $ \l -> placeProgress a l 3
      pure s
    _ -> ThePowerOfSpirit <$> runMessage msg a
