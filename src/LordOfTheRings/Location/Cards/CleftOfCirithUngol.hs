module LordOfTheRings.Location.Cards.CleftOfCirithUngol (cleftOfCirithUngol) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (placeResourceOnShelob, shelobTravelModifiers)

newtype CleftOfCirithUngol = CleftOfCirithUngol LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Set aside by stage 1A, added to the staging area by stage 3A. While it is in the
victory display, Shelob gets +2 attack, loses Indestructible, and gains "Shelob engages
the first player": Shelob reads it there (see "LordOfTheRings.Enemy.Cards.Shelob").
-}
cleftOfCirithUngol :: LocationCard CleftOfCirithUngol
cleftOfCirithUngol = location CleftOfCirithUngol Cards.cleftOfCirithUngol

-- | Immune to player card effects.
instance HasModifiersFor CleftOfCirithUngol where
  getModifiersFor (CleftOfCirithUngol a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    shelobTravelModifiers a

-- | Travel: Place 1 resource on Shelob to travel here.
instance RunMessage CleftOfCirithUngol where
  runMessage msg (CleftOfCirithUngol a) = case msg of
    TravelTo lid | lid == a.id -> do
      placeResourceOnShelob a
      CleftOfCirithUngol <$> runMessage msg a
    _ -> CleftOfCirithUngol <$> runMessage msg a
