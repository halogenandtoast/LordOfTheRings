module LordOfTheRings.Location.Cards.SouthronCamp (southronCamp) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype SouthronCamp = SouthronCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronCamp :: LocationCard SouthronCamp
southronCamp = location SouthronCamp Cards.southronCamp

-- | While Southron Camp is in the staging area, it gains: "Forced: At the end of the round,
-- remove all progress from each location in play."
instance HasAbilities SouthronCamp where
  getAbilities (SouthronCamp a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == StagingArea]

-- | Travel: Raise each player's threat by 1 to travel here.
instance RunMessage SouthronCamp where
  runMessage msg l@(SouthronCamp a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      locations <- traverse getLocation =<< select AnyLocation
      for_ locations \loc -> when (loc.progress > 0) $ removeTokens a loc.id Progress loc.progress
      pure l
    TravelTo lid | lid == a.id -> do
      eachPlayer (`raiseThreat` 1)
      SouthronCamp <$> runMessage msg a
    _ -> SouthronCamp <$> runMessage msg a
