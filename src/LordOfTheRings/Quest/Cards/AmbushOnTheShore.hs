module LordOfTheRings.Quest.Cards.AmbushOnTheShore (ambushOnTheShore) where

import LordOfTheRings.Matcher
import LordOfTheRings.Outcome
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AmbushOnTheShore = AmbushOnTheShore QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ambushOnTheShore :: QuestCard AmbushOnTheShore
ambushOnTheShore = quest AmbushOnTheShore Cards.ambushOnTheShore

-- | Skip the staging step of the quest phase for the remainder of the game.
instance HasModifiersFor AmbushOnTheShore where
  getModifiersFor (AmbushOnTheShore a) = modified a GameTarget [SkipStagingStep]

-- | When Revealed: Reveal 2 encounter cards per player, and add them to the staging area.
-- Once there are no enemies in play, the players have won the game.
instance RunMessage AmbushOnTheShore where
  runMessage msg q@(AmbushOnTheShore attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      order <- getPlayerOrder
      -- once the ambush is revealed, see whether any enemy came of it
      pushAll $ concatMap (const [RevealEncounterCard, RevealEncounterCard]) order <> [DoStep 1 (Flip $ toTarget attrs)]
      AmbushOnTheShore <$> runMessage msg attrs
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      none <- selectNone AnyEnemy
      pushWhen none $ GameOver Won
      pure q
    _ | Just eid <- leaving msg -> do
      others <- select (not_ $ EnemyWithId eid)
      pushWhen (null others) $ GameOver Won
      AmbushOnTheShore <$> runMessage msg attrs
    _ -> AmbushOnTheShore <$> runMessage msg attrs
   where
    leaving = \case
      Destroy (EnemyTarget eid) -> Just eid
      Discard (EnemyTarget eid) -> Just eid
      ReturnToTopOfEncounterDeck (EnemyTarget eid) -> Just eid
      _ -> Nothing
