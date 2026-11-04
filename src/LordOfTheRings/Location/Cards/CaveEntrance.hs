module LordOfTheRings.Location.Cards.CaveEntrance (caveEntrance) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (dragonSignsInVictoryDisplay)

newtype CaveEntrance = CaveEntrance LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Deep.
caveEntrance :: LocationCard CaveEntrance
caveEntrance = location CaveEntrance Cards.caveEntrance

-- | Cave Entrance gets +1 threat for each Dragon Sign in the victory display.
instance HasModifiersFor CaveEntrance where
  getModifiersFor (CaveEntrance a) = do
    x <- dragonSignsInVictoryDisplay
    when (x > 0) $ modifySelf a [ThreatModifier x]

-- | Travel: Raise each player's threat by 1 to travel here.
instance RunMessage CaveEntrance where
  runMessage msg (CaveEntrance a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer (`raiseThreat` 1)
      CaveEntrance <$> runMessage msg a
    _ -> CaveEntrance <$> runMessage msg a
