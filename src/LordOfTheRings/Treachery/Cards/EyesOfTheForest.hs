module LordOfTheRings.Treachery.Cards.EyesOfTheForest (eyesOfTheForest) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EyesOfTheForest = EyesOfTheForest TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eyesOfTheForest :: TreacheryCard EyesOfTheForest
eyesOfTheForest = treachery EyesOfTheForest Cards.eyesOfTheForest
