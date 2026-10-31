module LordOfTheRings.Location.Cards.OverrunVillage (overrunVillage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardVillagers, villagerDies, withVillagers)

newtype OverrunVillage = OverrunVillage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Villagers 2.
overrunVillage :: LocationCard OverrunVillage
overrunVillage = location (OverrunVillage . withVillagers 2) Cards.overrunVillage

-- | While Overrun Village is in the staging area, it gains: "Forced: After an enemy
-- engages a player, discard 1 villager token from Overrun Village."
instance HasAbilities OverrunVillage where
  getAbilities (OverrunVillage a) =
    [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.placement == StagingArea]

-- | Travel: Place 1 damage token on Dead Villagers.
instance RunMessage OverrunVillage where
  runMessage msg l@(OverrunVillage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      void $ discardVillagers a a 1
      pure l
    TravelTo lid | lid == a.id -> do
      villagerDies a
      pure l
    _ -> OverrunVillage <$> runMessage msg a
