module LordOfTheRings.Treachery.Cards.EyesOfTheForest (eyesOfTheForest) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EyesOfTheForest = EyesOfTheForest TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eyesOfTheForest :: TreacheryCard EyesOfTheForest
eyesOfTheForest = treachery EyesOfTheForest Cards.eyesOfTheForest

instance RunMessage EyesOfTheForest where
  runMessage msg t@(EyesOfTheForest attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer $ push . DiscardEventsFromHand
      pure t
    _ -> EyesOfTheForest <$> runMessage msg attrs
