module LordOfTheRings.Location.Cards.ComfortableLight (comfortableLight) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ComfortableLight = ComfortableLight LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

comfortableLight :: LocationCard ComfortableLight
comfortableLight = location ComfortableLight Cards.comfortableLight

instance HasModifiersFor ComfortableLight where
  getModifiersFor (ComfortableLight a) = modifySelf a [ProgressLimit 0, ProgressReducedTo 0]

instance HasAbilities ComfortableLight where
  getAbilities (ComfortableLight a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage ComfortableLight where
  runMessage msg l@(ComfortableLight attrs) = case msg of
    TravelTo lid | lid == attrs.id -> do
      sack 1
      ComfortableLight <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discard attrs attrs
      pure l
    _ -> ComfortableLight <$> runMessage msg attrs
