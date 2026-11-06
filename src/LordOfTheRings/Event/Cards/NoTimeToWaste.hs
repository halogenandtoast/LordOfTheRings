module LordOfTheRings.Event.Cards.NoTimeToWaste (noTimeToWaste) where

import LordOfTheRings.Campaign (CampaignLogUpdate (RemoveOneFromPool), PoolCard (..))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)

newtype NoTimeToWaste = NoTimeToWaste EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

noTimeToWaste :: EventCard NoTimeToWaste
noTimeToWaste = eventWith NoTimeToWaste Cards.noTimeToWaste (criteriaL .~ DuringPhase QuestPhase)

instance RunMessage NoTimeToWaste where
  runMessage msg e@(NoTimeToWaste a) = case msg of
    PlayEvent pid card _ | card.id == toId a -> do
      seat <- seatOf pid
      chooseOneM pid do
        questionLabeled "campaign.boonChoice"
        causedBy a
        labeled "campaign.noTimeToWaste" do
          removeFromGame a
          updateCampaignLog (RemoveOneFromPool $ PoolCard (toCardCode Cards.noTimeToWaste) seat Nothing)
          eachPlayer $ \p -> lowerThreat p 3
          revealEncounterCard
        labeled "campaign.drawOne" $ drawCards a pid 1
      pure e
    _ -> NoTimeToWaste <$> runMessage msg a
