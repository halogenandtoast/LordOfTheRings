module LordOfTheRings.Location.Cards.WallOfMorannon (wallOfMorannon) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype WallOfMorannon = WallOfMorannon LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

wallOfMorannon :: LocationCard WallOfMorannon
wallOfMorannon = location WallOfMorannon Cards.wallOfMorannon

-- | While Wall of Morannon is the active location, each enemy and location in the staging
-- area is immune to player card effects.
instance HasModifiersFor WallOfMorannon where
  getModifiersFor (WallOfMorannon a) = when (a.placement == ActiveLocation) do
    enemies <- select EnemyInStagingArea
    locations <- select LocationInStagingArea
    for_ enemies \e -> modified a e [ImmuneToPlayerCardEffects]
    for_ locations \l -> modified a l [ImmuneToPlayerCardEffects]

-- | Forced: After Wall of Morannon becomes the active location, return each engaged enemy
-- to the staging area.
instance HasAbilities WallOfMorannon where
  getAbilities (WallOfMorannon a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage WallOfMorannon where
  runMessage msg l@(WallOfMorannon a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnEngagedEnemiesToStaging
      pure l
    _ -> WallOfMorannon <$> runMessage msg a
