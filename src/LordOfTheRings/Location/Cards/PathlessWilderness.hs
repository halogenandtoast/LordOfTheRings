module LordOfTheRings.Location.Cards.PathlessWilderness (pathlessWilderness) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)

newtype PathlessWilderness = PathlessWilderness LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pathlessWilderness :: LocationCard PathlessWilderness
pathlessWilderness =
  location
    (PathlessWilderness . \a -> a {locationTokens = addTokens Resource 3 a.locationTokens})
    Cards.pathlessWilderness

instance HasAbilities PathlessWilderness where
  getAbilities (PathlessWilderness a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage PathlessWilderness where
  runMessage msg l@(PathlessWilderness a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeTokens a a Resource 1
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (countTokens Resource a.locationTokens == 0) $ reduceRingBearersLife 3
      pure l
    _ -> PathlessWilderness <$> runMessage msg a
