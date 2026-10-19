module LordOfTheRings.Enemy.Cards.DungeonJailor (dungeonJailor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DungeonJailor = DungeonJailor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dungeonJailor :: EnemyCard DungeonJailor
dungeonJailor = enemy DungeonJailor Cards.dungeonJailor

-- | Forced: If Dungeon Jailor is in the staging area after the players have just quested
-- unsuccessfully, shuffle 1 unclaimed objective card from the staging area back into the
-- encounter deck.
instance HasAbilities DungeonJailor where
  getAbilities (DungeonJailor a) =
    [restricted a 1 (EnemyExists $ EnemyWithId a.id <> EnemyInStagingArea) $ forced $ QuestedUnsuccessfullyWindow After]

instance RunMessage DungeonJailor where
  runMessage msg e@(DungeonJailor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      unclaimed <- getsGame (map toTarget . filter ((== StagingArea) . (.placement)) . toList . entitiesObjectives . gameEntities)
      chooseOrRunOneM lead $ targets unclaimed \o -> push $ ShuffleIntoEncounterDeck o
      pure e
    _ -> DungeonJailor <$> runMessage msg attrs
