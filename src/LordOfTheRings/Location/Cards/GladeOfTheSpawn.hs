module LordOfTheRings.Location.Cards.GladeOfTheSpawn (gladeOfTheSpawn) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Spider))

newtype GladeOfTheSpawn = GladeOfTheSpawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gladeOfTheSpawn :: LocationCard GladeOfTheSpawn
gladeOfTheSpawn = location GladeOfTheSpawn Cards.gladeOfTheSpawn

-- | When Glade of the Spawn is the active location, it gains: "Forced: After a Spider enemy
-- enters play, each player must choose and exhaust 1 character he controls."
instance HasAbilities GladeOfTheSpawn where
  -- ponytail: a Spider revealed, the way one nearly always enters play
  getAbilities (GladeOfTheSpawn a) =
    [mkAbility a 1 $ forced $ EnemyWithTraitRevealedWindow After Spider | a.placement == ActiveLocation]

instance RunMessage GladeOfTheSpawn where
  runMessage msg l@(GladeOfTheSpawn a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> chooseAndExhaust a pid 1
      pure l
    _ -> GladeOfTheSpawn <$> runMessage msg a
