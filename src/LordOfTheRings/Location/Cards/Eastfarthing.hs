module LordOfTheRings.Location.Cards.Eastfarthing (eastfarthing) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype Eastfarthing = Eastfarthing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

eastfarthing :: LocationCard Eastfarthing
eastfarthing = location Eastfarthing Cards.eastfarthing

instance HasModifiersFor Eastfarthing where
  getModifiersFor (Eastfarthing a) = modifySelf a [ThreatModifier (2 * countTokens Resource (locationTokens a))]

instance HasAbilities Eastfarthing where
  getAbilities (Eastfarthing a) = [mkAbility a 1 $ forced $ FailedHideTestWindow After AnyPlayer]

instance RunMessage Eastfarthing where
  runMessage msg l@(Eastfarthing a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      pure l
    _ -> Eastfarthing <$> runMessage msg a
