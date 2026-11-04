module LordOfTheRings.Location.Cards.DeepDarkPassage (deepDarkPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (exhaustSmeagolToTravel, smeagolTravelModifiers)

newtype DeepDarkPassage = DeepDarkPassage LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepDarkPassage :: LocationCard DeepDarkPassage
deepDarkPassage = location DeepDarkPassage Cards.deepDarkPassage

-- | While Deep Dark Passage is in the staging area, it gains: "Forced: When any amount of
-- progress would be placed on a location in the staging area, reduce that amount by 1."
-- Each copy in the staging area reduces it by 1.
-- Travel: Exhaust Sméagol to travel here.
instance HasModifiersFor DeepDarkPassage where
  getModifiersFor (DeepDarkPassage a) = do
    when (a.placement == StagingArea) do
      staged <- select LocationInStagingArea
      for_ staged \l -> modified a l [ProgressReducedBy 1]
    smeagolTravelModifiers a

instance RunMessage DeepDarkPassage where
  runMessage msg (DeepDarkPassage a) = case msg of
    TravelTo lid | lid == a.id -> do
      exhaustSmeagolToTravel
      DeepDarkPassage <$> runMessage msg a
    _ -> DeepDarkPassage <$> runMessage msg a
