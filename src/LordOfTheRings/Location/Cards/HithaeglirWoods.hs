module LordOfTheRings.Location.Cards.HithaeglirWoods (hithaeglirWoods) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Dunland))

newtype HithaeglirWoods = HithaeglirWoods LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hithaeglirWoods :: LocationCard HithaeglirWoods
hithaeglirWoods = location HithaeglirWoods Cards.hithaeglirWoods

-- | While Hithaeglir Woods is in the staging area, each Dunland enemy in the staging area
-- cannot take damage.
instance HasModifiersFor HithaeglirWoods where
  getModifiersFor (HithaeglirWoods a) = when (a.placement == StagingArea) do
    enemies <- select (EnemyWithTrait Dunland <> EnemyInStagingArea)
    for_ enemies \e -> modified a e [CannotTakeDamage]

-- | Travel: Deal each Dunland enemy a shadow card to travel here.
instance RunMessage HithaeglirWoods where
  runMessage msg l@(HithaeglirWoods a) = case msg of
    TravelTo lid | lid == a.id -> do
      traverse_ dealShadowCard =<< select (EnemyWithTrait Dunland)
      pure l
    _ -> HithaeglirWoods <$> runMessage msg a
