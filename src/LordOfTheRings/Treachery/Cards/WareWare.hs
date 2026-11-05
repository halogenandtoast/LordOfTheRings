module LordOfTheRings.Treachery.Cards.WareWare (wareWare) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCard, toCardCode)
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (getTheBlackGate, cardsUnderTheCrossRoads, putFromUnderTheCrossRoads)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WareWare = WareWare TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wareWare :: TreacheryCard WareWare
wareWare = treachery WareWare Cards.wareWare

instance RunMessage WareWare where
  runMessage msg t@(WareWare a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      pid <- getRevealer
      gate <- getTheBlackGate
      under <- cardsUnderTheCrossRoads
      chooseOrRunOneM pid do
        questionLabeled "wareWare.choose"
        causedBy a
        for_ gate \g -> labeled "wareWare.blackGate" do
          let card = MkEncounterCard a.cardId (toCardCode a)
          discard a a
          removeFromEncounterDiscard card
          placeUnderneath g [toCard card]
        unless (null under) $ labeled "wareWare.engage" $ putFromUnderTheCrossRoads pid
      pure t
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      enemies <- select EnemyInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "wareWare.enemy"
        causedBy a
        targets enemies \eid -> do
          engageEnemy pid eid
          dealShadowCard eid
      pure t
    _ -> WareWare <$> runMessage msg a
