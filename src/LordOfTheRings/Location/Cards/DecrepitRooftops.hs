module LordOfTheRings.Location.Cards.DecrepitRooftops (decrepitRooftops) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (returnEngagedEnemiesToStaging)

newtype DecrepitRooftops = DecrepitRooftops LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

decrepitRooftops :: LocationCard DecrepitRooftops
decrepitRooftops = location DecrepitRooftops Cards.decrepitRooftops

-- | While Decrepit Rooftops is the active location, enemies get +1 threat and do not make
-- engagement checks.
instance HasModifiersFor DecrepitRooftops where
  getModifiersFor (DecrepitRooftops a) =
    when (a.placement == ActiveLocation) do
      modified a GameTarget [NoEngagementChecks]
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [ThreatModifier 1]

-- | Forced: After traveling to Decrepit Rooftops, return all engaged enemies to the staging
-- area.
instance HasAbilities DecrepitRooftops where
  getAbilities (DecrepitRooftops a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage DecrepitRooftops where
  runMessage msg l@(DecrepitRooftops attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      returnEngagedEnemiesToStaging
      pure l
    _ -> DecrepitRooftops <$> runMessage msg attrs
