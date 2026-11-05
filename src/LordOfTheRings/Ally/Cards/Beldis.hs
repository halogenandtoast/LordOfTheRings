module LordOfTheRings.Ally.Cards.Beldis (beldis) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), CampaignLogUpdate (AddToCount), seatUseCode)
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype Beldis = Beldis AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Ered Mithrin campaign, from Fire in the Night.
beldis :: AllyCard Beldis
beldis = ally Beldis Cards.beldis

-- | Each player has 4 uses for the campaign.
uses :: Int
uses = 4

{- | Action: Spend 1 resource and mark a use in the campaign log to heal 3 damage from a
character. Any player may trigger this ability. Outside campaign mode there is no log to
mark a use in, so it cannot be triggered.
-}
instance HasAbilities Beldis where
  getAbilities (Beldis a) =
    [ anyPlayer
        $ restricted a 1 (PlayerExists $ ActivePlayer <> PlayerWithCampaignUsesBelow (toCardCode Cards.beldis) uses)
        $ ActionAbility (SpendResourcesCost Neutral 1)
    ]

instance RunMessage Beldis where
  runMessage msg s@(Beldis a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      seat <- seatOf pid
      for_ seat \n -> updateCampaignLog (AddToCount CardUses (seatUseCode (toCardCode Cards.beldis) n) 1)
      characters <- selectCharacters AnyHero AnyAlly
      damaged <- filterM (fmap (> 0) . getDamage) characters
      chooseOrRunOneM pid do
        questionLabeled "beldis.character"
        causedBy a
        targets damaged \c -> healDamage a c 3
      pure s
    _ -> Beldis <$> runMessage msg a
