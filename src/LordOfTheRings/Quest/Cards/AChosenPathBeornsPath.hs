module LordOfTheRings.Quest.Cards.AChosenPathBeornsPath (aChosenPathBeornsPath) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AChosenPathBeornsPath = AChosenPathBeornsPath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aChosenPathBeornsPath :: QuestCard AChosenPathBeornsPath
aChosenPathBeornsPath = quest AChosenPathBeornsPath Cards.aChosenPathBeornsPath

-- | Players cannot defeat this stage while Ungoliant's Spawn is in play. If players defeat
-- this stage, they have won the game (it is the last stage, so defeating it wins).
instance RunMessage AChosenPathBeornsPath where
  runMessage msg (AChosenPathBeornsPath attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      blocked <- selectAny spawn
      if blocked
        then pure $ AChosenPathBeornsPath $ attrs & tokensL %~ addTokens Progress n
        else AChosenPathBeornsPath <$> runMessage msg attrs
    -- progress that piled up while the Spawn was in play counts once it is gone
    _ | Just eid <- spawnLeaving msg -> do
      leaving <- selectAny (EnemyWithId eid <> spawn)
      -- the one leaving is still counted: no other copy may stay behind
      spawns <- length <$> select spawn
      when (leaving && spawns == 1 && attrs.questPoints > 0 && attrs.progress >= attrs.questPoints)
        $ push (CompleteQuest attrs.id)
      AChosenPathBeornsPath <$> runMessage msg attrs
    _ -> AChosenPathBeornsPath <$> runMessage msg attrs
   where
    spawn = EnemyWithCardCode (toCardCode Enemies.ungoliantsSpawn)
    spawnLeaving = \case
      Destroy (EnemyTarget eid) -> Just eid
      Discard (EnemyTarget eid) -> Just eid
      _ -> Nothing
