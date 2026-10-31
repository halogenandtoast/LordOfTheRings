module LordOfTheRings.Location.Cards.DawnStarLocation (dawnStarLocation) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Raid on the Grey Havens Nightmare's Dawn Star, a location: not the fleet's Ship
-- objective.
newtype DawnStarLocation = DawnStarLocation LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dawnStarLocation :: LocationCard DawnStarLocation
dawnStarLocation = location DawnStarLocation Cards.dawnStarLocation

-- | Immune to player card effects.
instance HasModifiersFor DawnStarLocation where
  getModifiersFor (DawnStarLocation a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After the first player draws any number of cards, deal 1 damage to the Dawn
-- Star.
instance RunMessage DawnStarLocation where
  runMessage msg (DawnStarLocation a) = case msg of
    CheckWindows ws -> do
      lead <- getLead
      for_ [() | Window After (CardsDrawn pid _) <- ws, pid == lead] \_ -> dealDamage a a.id 1
      DawnStarLocation <$> runMessage msg a
    _ -> DawnStarLocation <$> runMessage msg a
