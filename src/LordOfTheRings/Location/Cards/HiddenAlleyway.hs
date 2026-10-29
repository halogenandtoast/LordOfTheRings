module LordOfTheRings.Location.Cards.HiddenAlleyway (hiddenAlleyway) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype HiddenAlleyway = HiddenAlleyway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hiddenAlleyway :: LocationCard HiddenAlleyway
hiddenAlleyway = location HiddenAlleyway Cards.hiddenAlleyway

-- | Forced: After traveling to Hidden Alleyway, add 1 time counter to the current quest.
instance HasAbilities HiddenAlleyway where
  getAbilities (HiddenAlleyway a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Travel: Raise each player's threat by X to travel here. X is the number of enemies in
-- play.
instance RunMessage HiddenAlleyway where
  runMessage msg l@(HiddenAlleyway attrs) = case msg of
    TravelTo lid | lid == attrs.id -> do
      x <- length <$> select AnyEnemy
      when (x > 0) $ eachPlayer (`raiseThreat` x)
      HiddenAlleyway <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeTimeCountersOnCurrentQuest attrs 1
      pure l
    _ -> HiddenAlleyway <$> runMessage msg attrs
