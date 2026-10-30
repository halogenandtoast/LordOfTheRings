module LordOfTheRings.Ally.Cards.CorsairSkirmisherShip (corsairSkirmisherShip) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype CorsairSkirmisherShip = CorsairSkirmisherShip AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Corsair Skirmisher taken by the players as a Ship objective, with willpower equal to
-- its threat.
corsairSkirmisherShip :: AllyCard CorsairSkirmisherShip
corsairSkirmisherShip = ally CorsairSkirmisherShip Cards.corsairSkirmisherShip

-- | Immune to player card effects.
instance HasModifiersFor CorsairSkirmisherShip where
  getModifiersFor (CorsairSkirmisherShip a) = shipModifiers a

-- | Destroyed, it leaves the game, as a Ship objective of the fleet.
instance RunMessage CorsairSkirmisherShip where
  runMessage msg (CorsairSkirmisherShip a) = CorsairSkirmisherShip <$> runShip (pure ()) msg a
