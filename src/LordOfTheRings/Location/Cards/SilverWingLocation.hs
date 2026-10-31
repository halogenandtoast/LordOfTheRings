module LordOfTheRings.Location.Cards.SilverWingLocation (silverWingLocation) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Raid on the Grey Havens Nightmare's Silver Wing, a location: not the fleet's Ship
-- objective.
newtype SilverWingLocation = SilverWingLocation LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

silverWingLocation :: LocationCard SilverWingLocation
silverWingLocation = location SilverWingLocation Cards.silverWingLocation

-- | Immune to player card effects.
instance HasModifiersFor SilverWingLocation where
  getModifiersFor (SilverWingLocation a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After the first player attacks an enemy, deal 1 damage to the Silver Wing: once
-- for each attack, however many of his characters made it.
instance RunMessage SilverWingLocation where
  runMessage msg (SilverWingLocation a) = case msg of
    CheckWindows ws -> do
      lead <- getLead
      attacks <- ordNub . concat <$> for [(c, eid) | Window After (AttackedEnemy c eid) <- ws] \(c, eid) ->
        (\p -> [eid | p == Just lead]) <$> characterController c
      for_ attacks \_ -> dealDamage a a.id 1
      SilverWingLocation <$> runMessage msg a
    _ -> SilverWingLocation <$> runMessage msg a
