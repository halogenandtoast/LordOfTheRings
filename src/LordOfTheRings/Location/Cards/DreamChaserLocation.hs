module LordOfTheRings.Location.Cards.DreamChaserLocation (dreamChaserLocation) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Raid on the Grey Havens' Dream-chaser. Placed underneath The Havens Burn, the players
-- lose the game: "LordOfTheRings.Objective.Cards.TheHavensBurn" reads it.
newtype DreamChaserLocation = DreamChaserLocation LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dreamChaserLocation :: LocationCard DreamChaserLocation
dreamChaserLocation = location DreamChaserLocation Cards.dreamChaserLocation

-- | Immune to player card effects. The players cannot travel to The Dream-chaser.
instance HasModifiersFor DreamChaserLocation where
  getModifiersFor (DreamChaserLocation a) = modifySelf a [ImmuneToPlayerCardEffects, CannotBeTraveledTo]

instance RunMessage DreamChaserLocation where
  runMessage msg (DreamChaserLocation a) = DreamChaserLocation <$> runMessage msg a
