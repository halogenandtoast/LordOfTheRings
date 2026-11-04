module LordOfTheRings.Location.Cards.HiddenPath (hiddenPath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype HiddenPath = HiddenPath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hiddenPath :: LocationCard HiddenPath
hiddenPath = location HiddenPath Cards.hiddenPath

-- | While Hidden Path is the active location, locations in the staging area get -1 threat.
-- Travel: Spend 2 Baggins resources to travel here: the players' as a group.
instance HasModifiersFor HiddenPath where
  getModifiersFor (HiddenPath a) = do
    when (a.placement == ActiveLocation) do
      staged <- select LocationInStagingArea
      for_ staged \l -> modified a l [ThreatModifier (-1)]
    baggins <- getGroupResourcesOfSphere Baggins
    when (baggins < 2) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage HiddenPath where
  runMessage msg (HiddenPath a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      spendGroupResources lead Baggins 2
      HiddenPath <$> runMessage msg a
    _ -> HiddenPath <$> runMessage msg a
