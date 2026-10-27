module LordOfTheRings.Location.Cards.FirstHall (firstHall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FirstHall = FirstHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

firstHall :: LocationCard FirstHall
firstHall = location FirstHall Cards.firstHall

-- | Forced: After First Hall leaves play as an explored location, add Bridge of
-- Khazad-dûm to the staging area.
instance HasAbilities FirstHall where
  getAbilities (FirstHall a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Travel: Each player must raise his threat by 3 to travel here.
instance RunMessage FirstHall where
  runMessage msg l@(FirstHall a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer (`raiseThreat` 3)
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      addSetAsideToStaging (cardIs Cards.bridgeOfKhazadDum)
      pure l
    _ -> FirstHall <$> runMessage msg a
