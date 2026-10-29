module LordOfTheRings.Location.Cards.RuinsOfTheSecondAge (ruinsOfTheSecondAge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Trait (Trait (City))

newtype RuinsOfTheSecondAge = RuinsOfTheSecondAge LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinsOfTheSecondAge :: LocationCard RuinsOfTheSecondAge
ruinsOfTheSecondAge = location RuinsOfTheSecondAge Cards.ruinsOfTheSecondAge

-- | While Ruins of the Second Age is in the staging area, City locations get +1 threat.
instance HasModifiersFor RuinsOfTheSecondAge where
  getModifiersFor (RuinsOfTheSecondAge a) =
    when (a.placement == StagingArea) do
      cities <- select (LocationWithTrait City)
      for_ cities \l -> modified a l [ThreatModifier 1]

-- | While Ruins of the Second Age is the active location, deal engaged enemies 1 additional
-- shadow card at the beginning of the combat phase.
instance HasAbilities RuinsOfTheSecondAge where
  getAbilities (RuinsOfTheSecondAge a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After CombatPhase | a.placement == ActiveLocation]

instance RunMessage RuinsOfTheSecondAge where
  runMessage msg l@(RuinsOfTheSecondAge attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      engaged <- select (EnemyEngagedWith AnyPlayer)
      for_ engaged dealShadowCard
      pure l
    _ -> RuinsOfTheSecondAge <$> runMessage msg attrs
