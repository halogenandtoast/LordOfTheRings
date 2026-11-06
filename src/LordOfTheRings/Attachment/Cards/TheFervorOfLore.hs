module LordOfTheRings.Attachment.Cards.TheFervorOfLore (theFervorOfLore) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), CampaignLogUpdate (AddToCount))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Sphere (Sphere (Lore))

newtype TheFervorOfLore = TheFervorOfLore AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

theFervorOfLore :: AttachmentCard TheFervorOfLore
theFervorOfLore = attachment TheFervorOfLore Cards.theFervorOfLore & attachTo (AttachToHero AnyHero)

instance HasModifiersFor TheFervorOfLore where
  getModifiersFor (TheFervorOfLore a) = withHost a $ \host -> modified a host [GainResourceIcon Lore]

-- The campaign log keeps the card-wide use across games and changes of controller.
instance HasAbilities TheFervorOfLore where
  getAbilities (TheFervorOfLore a) =
    [ restricted
        a
        1
        ( PlayerExists
            (PlayerWithId a.controller <> PlayerWithCampaignCardUsesBelow (toCardCode Cards.theFervorOfLore) 1)
            <> HeroExists DamagedHero
        )
        $ ActionAbility (DiscardFromHandCost a.controller 3)
    | AttachedTo (HeroTarget _) <- [a.placement]
    ]

instance RunMessage TheFervorOfLore where
  runMessage msg s@(TheFervorOfLore a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      updateCampaignLog (AddToCount CardUses (toCardCode Cards.theFervorOfLore) 1)
      heroes <- select DamagedHero
      chooseOrRunOneM pid do
        questionLabeled "campaign.adaptationTarget"
        targets heroes $ healAllDamage a
      pure s
    _ -> TheFervorOfLore <$> runMessage msg a
