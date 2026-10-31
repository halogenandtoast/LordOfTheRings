module LordOfTheRings.Location.Cards.RuinedWatchTower (ruinedWatchTower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Trait (Trait (Orc))

newtype RuinedWatchTower = RuinedWatchTower LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinedWatchTower :: LocationCard RuinedWatchTower
ruinedWatchTower = location RuinedWatchTower Cards.ruinedWatchTower

-- | While Ruined Watch-tower is the active location, each enemy gets +3 defense.
instance HasModifiersFor RuinedWatchTower where
  getModifiersFor (RuinedWatchTower a) = when (a.placement == ActiveLocation) do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [DefenseModifier 3]

-- | While Ruined Watch-tower is in the staging area, it gains: "Forced: When an Orc enemy
-- is revealed, each player must deal 1 damage to a character he controls."
instance HasAbilities RuinedWatchTower where
  getAbilities (RuinedWatchTower a) = [mkAbility a 1 $ forced $ EnemyWithTraitRevealedWindow When Orc | a.placement == StagingArea]

instance RunMessage RuinedWatchTower where
  runMessage msg l@(RuinedWatchTower a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> dealDamageToOneCharacter a p 1
      pure l
    _ -> RuinedWatchTower <$> runMessage msg a
