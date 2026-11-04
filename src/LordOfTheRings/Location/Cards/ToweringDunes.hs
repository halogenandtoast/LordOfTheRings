module LordOfTheRings.Location.Cards.ToweringDunes (toweringDunes) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype ToweringDunes = ToweringDunes LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

toweringDunes :: LocationCard ToweringDunes
toweringDunes = location ToweringDunes Cards.toweringDunes

-- | While Towering Dunes is in the staging area, progress cannot be placed on locations in
-- the staging area not named Towering Dunes.
instance HasModifiersFor ToweringDunes where
  getModifiersFor (ToweringDunes a) = when (a.placement == StagingArea) do
    others <- select (LocationInStagingArea <> not_ (LocationWithCardCode $ toCardCode a))
    for_ others \l -> modified a l [ProgressLimit 0]

-- | Forced: After Towering Dunes becomes the active location, reveal the top card of the
-- encounter deck.
instance HasAbilities ToweringDunes where
  getAbilities (ToweringDunes a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage ToweringDunes where
  runMessage msg l@(ToweringDunes a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      revealEncounterCard
      pure l
    _ -> ToweringDunes <$> runMessage msg a
