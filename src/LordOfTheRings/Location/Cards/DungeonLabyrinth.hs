module LordOfTheRings.Location.Cards.DungeonLabyrinth (dungeonLabyrinth) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DungeonLabyrinth = DungeonLabyrinth LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

dungeonLabyrinth :: LocationCard DungeonLabyrinth
dungeonLabyrinth = location DungeonLabyrinth Cards.dungeonLabyrinth

-- | Dungeon Labyrinth gets +1 threat and +1 quest point for each resource token on it.
instance HasModifiersFor DungeonLabyrinth where
  getModifiersFor (DungeonLabyrinth a) = do
    let n = countTokens Resource a.locationTokens
    when (n > 0) $ modifySelf a [ThreatModifier n, QuestPointModifier n]

-- | Forced: At the end of each round, place 1 resource token on Dungeon Labyrinth for each
-- player in the game.
instance HasAbilities DungeonLabyrinth where
  getAbilities (DungeonLabyrinth a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage DungeonLabyrinth where
  runMessage msg l@(DungeonLabyrinth a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      placeResources a a players
      pure l
    _ -> DungeonLabyrinth <$> runMessage msg a
