module LordOfTheRings.Location.Cards.GuardedArchway (guardedArchway) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (cityGuard, getCityGuard)

newtype GuardedArchway = GuardedArchway LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardedArchway :: LocationCard GuardedArchway
guardedArchway = location GuardedArchway Cards.guardedArchway

-- | While Guarded Archway is in the staging area, each player engaged with City Guard
-- cannot reduce his threat.
instance HasModifiersFor GuardedArchway where
  getModifiersFor (GuardedArchway a) = when (a.placement == StagingArea) do
    players <- getRemainingPlayers
    for_ players \p -> whenM (selectAny $ cityGuard <> EnemyEngagedWith (PlayerWithId p)) do
      modified a (PlayerTarget p) [CannotReduceThreat]

-- | Travel: City Guard makes an attack against the first player.
instance RunMessage GuardedArchway where
  runMessage msg (GuardedArchway a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      traverse_ (immediateAttack lead) =<< getCityGuard
      GuardedArchway <$> runMessage msg a
    _ -> GuardedArchway <$> runMessage msg a
