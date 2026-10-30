module LordOfTheRings.Location.Cards.JaggedReef (jaggedReef) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (TravelPhase))

newtype JaggedReef = JaggedReef LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

jaggedReef :: LocationCard JaggedReef
jaggedReef = location JaggedReef Cards.jaggedReef

-- | Surge.
-- Forced: At the beginning of the travel phase, if you are on-course, place 2 progress on
-- Jagged Reef. If you are off-course, deal 2 damage to each Ship objective instead.
instance HasAbilities JaggedReef where
  getAbilities (JaggedReef a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After TravelPhase]

instance RunMessage JaggedReef where
  runMessage msg l@(JaggedReef a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      onCourse <- isOnCourse
      if onCourse
        then placeProgress a a 2
        else do
          ships <- select shipObjective
          for_ ships \s -> dealDamage a s 2
      pure l
    _ -> JaggedReef <$> runMessage msg a
