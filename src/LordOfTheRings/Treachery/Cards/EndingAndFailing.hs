module LordOfTheRings.Treachery.Cards.EndingAndFailing (endingAndFailing) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EndingAndFailing = EndingAndFailing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

endingAndFailing :: TreacheryCard EndingAndFailing
endingAndFailing = treachery EndingAndFailing Cards.endingAndFailing

instance RunMessage EndingAndFailing where
  runMessage msg t@(EndingAndFailing a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      phaseModifier a GameTarget RaiseThreatByStagingEnemiesAtEndOfPhase
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> EndingAndFailing <$> runMessage msg a
