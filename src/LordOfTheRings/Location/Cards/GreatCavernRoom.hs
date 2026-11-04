module LordOfTheRings.Location.Cards.GreatCavernRoom (greatCavernRoom) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim.Helpers (greatGoblinDefeated)
import LordOfTheRings.Trait (Trait (Goblin))

newtype GreatCavernRoom = GreatCavernRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

greatCavernRoom :: LocationCard GreatCavernRoom
greatCavernRoom = location GreatCavernRoom Cards.greatCavernRoom

-- | Forced: When faced with the option to travel, if The Great Goblin is in the victory
-- display the players must travel to Great Cavern Room if able.
instance HasModifiersFor GreatCavernRoom where
  getModifiersFor (GreatCavernRoom a) = whenM greatGoblinDefeated $ modifySelf a [MustBeTraveledTo]

-- | While Great Cavern Room is the active location, it gains: "Forced: After a player
-- engages a Goblin enemy, he must deal 1 damage to a character he controls."
instance HasAbilities GreatCavernRoom where
  getAbilities (GreatCavernRoom a) =
    [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow After (EnemyWithTrait Goblin) | a.placement == ActiveLocation]

instance RunMessage GreatCavernRoom where
  runMessage msg l@(GreatCavernRoom a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      goblins <- select (EnemyWithTrait Goblin)
      for_ [pid | Window _ (EnemyEngaged eid pid) <- ws, eid `elem` goblins] \pid -> dealDamageToOneCharacter a pid 1
      pure l
    _ -> GreatCavernRoom <$> runMessage msg a
