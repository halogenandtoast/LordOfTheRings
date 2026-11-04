module LordOfTheRings.Location.Cards.HauntedHall (hauntedHall) where

import LordOfTheRings.Helpers.Window (playerCardController)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Dark))

newtype HauntedHall = HauntedHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hauntedHall :: LocationCard HauntedHall
hauntedHall = location HauntedHall Cards.hauntedHall

-- | Discover 4. While Haunted Hall is in the staging area, it gains, "Forced: After a
-- player places any amount of progress on a Dark location in the staging area, he raises
-- his threat by 1."
-- The player is the one whose card placed it.
instance HasAbilities HauntedHall where
  getAbilities (HauntedHall a) =
    [ mkAbility a 1 $ forced $ PlayerCardPlacedProgressWindow After (LocationInStagingArea <> LocationWithTrait Dark)
    | a.placement == StagingArea
    ]

instance RunMessage HauntedHall where
  runMessage msg l@(HauntedHall a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [source | Window After (ProgressPlacedOnLocationBy source _) <- ws] \source ->
        traverse_ (\pid -> encounterRaisesThreat pid 1) =<< playerCardController source
      pure l
    _ -> HauntedHall <$> runMessage msg a
