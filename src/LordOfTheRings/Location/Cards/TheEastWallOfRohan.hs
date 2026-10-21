module LordOfTheRings.Location.Cards.TheEastWallOfRohan (theEastWallOfRohan) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Rohan))

newtype TheEastWallOfRohan = TheEastWallOfRohan LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEastWallOfRohan :: LocationCard TheEastWallOfRohan
theEastWallOfRohan = location TheEastWallOfRohan Cards.theEastWallOfRohan

-- | While The East Wall of Rohan is the active location, non-Rohan characters cost 2
-- additional matching resources to play.
instance HasModifiersFor TheEastWallOfRohan where
  getModifiersFor (TheEastWallOfRohan a) =
    when (a.placement == ActiveLocation)
      $ modified a GameTarget [IncreaseCost (CardWithType AllyType <> NotCard (CardWithTrait Rohan)) 2]
