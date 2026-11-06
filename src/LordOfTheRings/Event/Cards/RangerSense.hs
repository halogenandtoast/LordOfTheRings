module LordOfTheRings.Event.Cards.RangerSense (rangerSense) where

import LordOfTheRings.Campaign (CampaignLogUpdate (RemoveOneFromPool), PoolCard (..))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)

newtype RangerSense = RangerSense EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

rangerSense :: EventCard RangerSense
rangerSense = event RangerSense Cards.rangerSense

instance RunMessage RangerSense where
  runMessage msg e@(RangerSense a) = case msg of
    PlayEvent pid card _ | card.id == toId a -> do
      seat <- seatOf pid
      deck <- take 4 . (.deck) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "campaign.boonChoice"
        causedBy a
        labeled "campaign.rangerSense" do
          removeFromGame a
          updateCampaignLog (RemoveOneFromPool $ PoolCard (toCardCode Cards.rangerSense) seat Nothing)
          chooseOrRunOneM pid do
            questionLabeled "campaign.rangerSenseTake"
            showing deck
            targets deck $ \c -> do
              push $ TakeFromDeck pid c.id
              returnToTopOfDeckInAnyOrder pid a (filter ((/= c.id) . (.id)) deck)
        labeled "campaign.drawOne" $ drawCards a pid 1
      pure e
    _ -> RangerSense <$> runMessage msg a
