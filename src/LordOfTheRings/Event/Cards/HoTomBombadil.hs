module LordOfTheRings.Event.Cards.HoTomBombadil (hoTomBombadil) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Campaign (CampaignLogUpdate (RemoveFromPool))
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype HoTomBombadil = HoTomBombadil EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, Entity)

hoTomBombadil :: EventCard HoTomBombadil
hoTomBombadil = eventWith HoTomBombadil Cards.hoTomBombadil ((windowL ?~ CardRevealedWindow When) . (criteriaL .~ whenRevealedCancelable))

instance HasModifiersFor HoTomBombadil where
  getModifiersFor (HoTomBombadil a) = do
    logbook <- getCampaignLog
    let earned = any ((== toCardCode Cards.hoTomBombadil) . (.code)) $ foldMap (.pool) logbook
    unless earned $ modified a (PlayerTarget a.owner) [CannotPlay $ cardIs Cards.hoTomBombadil]

instance RunMessage HoTomBombadil where
  runMessage msg e@(HoTomBombadil a) = case msg of
    PlayEvent pid card windows | card.id == toId a -> do
      addToVictoryDisplay card
      updateCampaignLog $ RemoveFromPool $ toCardCode Cards.hoTomBombadil
      for_ [target | Window _ (Revealed target) <- windows] (cancelWhenRevealed pid)
      pure e
    _ -> HoTomBombadil <$> runMessage msg a
