module LordOfTheRings.Location.Cards.HiddenEyot (hiddenEyot) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype HiddenEyot = HiddenEyot LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hiddenEyot :: LocationCard HiddenEyot
hiddenEyot = location HiddenEyot Cards.hiddenEyot

-- | Response: After Hidden Eyot leaves play as an explored location, place 2 time counters
-- on the current quest.
instance HasAbilities HiddenEyot where
  getAbilities (HiddenEyot a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

-- | Travel: Each player must exhaust a character he controls to travel here.
instance RunMessage HiddenEyot where
  runMessage msg l@(HiddenEyot a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeTimeCountersOnCurrentQuest a 2
      pure l
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> chooseAndExhaust a p 1
      pure l
    _ -> HiddenEyot <$> runMessage msg a
