module LordOfTheRings.Objective.Cards.DeadVillagers (deadVillagers) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (alcaronMayRedirect)

newtype DeadVillagers = DeadVillagers ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadVillagers :: ObjectiveCard DeadVillagers
deadVillagers = objective DeadVillagers Cards.deadVillagers

-- | If a villager token is discarded from a location, objective or quest stage, place a
-- damage token on Dead Villagers: unless Lord Alcaron places it on a location instead.
-- (At the end of the game, if there are more damage tokens here than villager tokens on
-- Rescued Villagers, the players have lost: see the scenario.)
instance RunMessage DeadVillagers where
  runMessage msg o@(DeadVillagers attrs) = case msg of
    DoStep 1 (RemoveTokens source _ Villager _) -> do
      unlessM alcaronMayRedirect $ dealDamage source attrs 1
      pure o
    -- Lord Alcaron's controller declined to place it
    DoStep 2 (RemoveTokens source _ Villager _) -> do
      dealDamage source attrs 1
      pure o
    _ -> DeadVillagers <$> runMessage msg attrs
