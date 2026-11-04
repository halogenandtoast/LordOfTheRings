module LordOfTheRings.Quest.Cards.FaceTheDragon (faceTheDragon) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (getDagnir)

newtype FaceTheDragon = FaceTheDragon QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
faceTheDragon :: QuestCard FaceTheDragon
faceTheDragon = quest FaceTheDragon Cards.faceTheDragon

{- | Forced: After Dagnir attacks and destroys a character, heal 5 damage from her, read as
her attack resolves.
Forced: After Face the Dragon is defeated, deal 5 damage to Dagnir: a quest card's
damage, which her limit does not hold back.
-}
instance RunMessage FaceTheDragon where
  runMessage msg (FaceTheDragon attrs) = case msg of
    CheckWindows ws -> do
      getDagnir >>= traverse_ \d ->
        when (or [eid == d | Window After (EnemyAttacking eid _) <- ws]) do
          whenM (attackDestroyedACharacter d) $ healDamage attrs (EnemyTarget d) 5
      FaceTheDragon <$> runMessage msg attrs
    CompleteQuest qid | qid == attrs.id -> do
      getDagnir >>= traverse_ \d -> dealDamage attrs (EnemyTarget d) 5
      FaceTheDragon <$> runMessage msg attrs
    _ -> FaceTheDragon <$> runMessage msg attrs
