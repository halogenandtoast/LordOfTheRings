module LordOfTheRings.Objective.Cards.TheHeroesDefense (theHeroesDefense) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (gateInStaging, getUrdug)

newtype TheHeroesDefense = TheHeroesDefense ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

theHeroesDefense :: ObjectiveCard TheHeroesDefense
theHeroesDefense = objective TheHeroesDefense Cards.theHeroesDefense

-- | While there are X or more resources on The Heroes' Defense, Urdug is considered to be
-- engaged with each player. X is five times the number of players.
instance HasModifiersFor TheHeroesDefense where
  getModifiersFor (TheHeroesDefense a) = do
    players <- length <$> getRemainingPlayers
    when (a.resources >= 5 * players) $ getUrdug >>= traverse_ \u -> modified a u [EngagedWithEachPlayer AnyPlayer]

-- | Response: After an enemy is destroyed, place 1 resource here. Not while Gate of
-- Gundabad is in the staging area, when it could place none.
instance HasAbilities TheHeroesDefense where
  getAbilities (TheHeroesDefense a) =
    [anyPlayer $ restricted a 1 (NotCriterion $ LocationExists gateInStaging) $ response $ EnemyDefeatedWindow After]

instance RunMessage TheHeroesDefense where
  runMessage msg o@(TheHeroesDefense attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure o
    PlaceTokens _ (isTarget attrs -> True) Resource _ -> do
      barred <- selectAny gateInStaging
      if barred then pure o else TheHeroesDefense <$> runMessage msg attrs
    _ -> TheHeroesDefense <$> runMessage msg attrs
