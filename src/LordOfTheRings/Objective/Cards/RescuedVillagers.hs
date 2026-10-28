module LordOfTheRings.Objective.Cards.RescuedVillagers (rescuedVillagers) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardVillagers, getVillagers)

newtype RescuedVillagers = RescuedVillagers ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

rescuedVillagers :: ObjectiveCard RescuedVillagers
rescuedVillagers = objective RescuedVillagers Cards.rescuedVillagers

-- | It takes no damage: what an undefended attack would deal it at Protect the Villagers
-- discards its villagers instead.
instance HasModifiersFor RescuedVillagers where
  getModifiersFor (RescuedVillagers a) = modified a a [CannotTakeDamage]

-- | If a location leaves play as an explored location, move any villager tokens from that
-- location to Rescued Villagers.
-- (At the end of the game, if there are more villager tokens here than damage tokens on
-- Dead Villagers, the players have won: see the scenario.)
instance RunMessage RescuedVillagers where
  runMessage msg o@(RescuedVillagers attrs) = case msg of
    CheckWindows windows | lids@(_ : _) <- [lid | Window When (Explored lid) <- windows] -> do
      for_ lids \lid -> do
        n <- getVillagers (toTarget lid)
        when (n > 0) do
          removeTokens attrs lid Villager n
          placeTokens attrs attrs Villager n
      pure o
    DealDamage source (isTarget attrs -> True) n -> do
      void $ discardVillagers source attrs n
      pure o
    _ -> RescuedVillagers <$> runMessage msg attrs
