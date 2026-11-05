module LordOfTheRings.Treachery.Cards.ComeBack (comeBack) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)

newtype ComeBack = ComeBack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

comeBack :: TreacheryCard ComeBack
comeBack = treachery ComeBack Cards.comeBack

instance RunMessage ComeBack where
  runMessage msg t@(ComeBack a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      reduceRingBearersLife 1
      removeProgressFromQuest a maxBound
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      chooseOneM pid do
        questionLabeled "comeBack.choose"
        causedBy a
        labeled "comeBack.life" $ reduceRingBearersLife 1
        labeled "comeBack.undefended" $ makeUndefended pid eid
      pure t
    _ -> ComeBack <$> runMessage msg a
