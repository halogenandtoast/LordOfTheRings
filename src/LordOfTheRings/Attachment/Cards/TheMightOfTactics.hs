module LordOfTheRings.Attachment.Cards.TheMightOfTactics (theMightOfTactics) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), CampaignLogUpdate (AddToCount))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype TheMightOfTactics = TheMightOfTactics AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

theMightOfTactics :: AttachmentCard TheMightOfTactics
theMightOfTactics = attachment TheMightOfTactics Cards.theMightOfTactics & attachTo (AttachToHero AnyHero)

instance HasModifiersFor TheMightOfTactics where
  getModifiersFor (TheMightOfTactics a) = withHost a $ \host -> modified a host [GainResourceIcon Tactics]

-- The campaign log keeps the card-wide use across games and changes of controller.
instance HasAbilities TheMightOfTactics where
  getAbilities (TheMightOfTactics a) =
    [ restricted
        a
        1
        ( PlayerExists
            (PlayerWithId a.controller <> PlayerWithCampaignCardUsesBelow (toCardCode Cards.theMightOfTactics) 1)
            <> EnemyExists (EnemyEngagedWith $ PlayerWithId a.controller)
        )
        $ ActionAbility (DamageHeroCost (HeroWithId hid) 1)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage TheMightOfTactics where
  runMessage msg s@(TheMightOfTactics a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      updateCampaignLog (AddToCount CardUses (toCardCode Cards.theMightOfTactics) 1)
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "campaign.adaptationTarget"
        targets enemies $ \e -> dealDamage a e 3
      pure s
    _ -> TheMightOfTactics <$> runMessage msg a
