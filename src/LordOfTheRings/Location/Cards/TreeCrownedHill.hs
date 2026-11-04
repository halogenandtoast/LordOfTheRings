module LordOfTheRings.Location.Cards.TreeCrownedHill (treeCrownedHill) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (exhaustCharactersAsGroup)

newtype TreeCrownedHill = TreeCrownedHill LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

treeCrownedHill :: LocationCard TreeCrownedHill
treeCrownedHill = location TreeCrownedHill Cards.treeCrownedHill

-- | While Tree-crowned Hill is the active location, each enemy gets -1 threat.
instance HasModifiersFor TreeCrownedHill where
  getModifiersFor (TreeCrownedHill a) = when (a.placement == ActiveLocation) do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [ThreatModifier (-1)]

-- | Forced: When Tree-crowned Hill is explored, the players, as a group, exhaust X
-- characters in play. X is the number of damage here.
instance HasAbilities TreeCrownedHill where
  getAbilities (TreeCrownedHill a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage TreeCrownedHill where
  runMessage msg l@(TreeCrownedHill a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      exhaustCharactersAsGroup a a.damage
      pure l
    _ -> TreeCrownedHill <$> runMessage msg a
