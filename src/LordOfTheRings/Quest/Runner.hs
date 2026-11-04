{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Quest.Runner (module LordOfTheRings.Quest.Runner, module X) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (modifiedValue, questPointsOf)
import LordOfTheRings.I18n
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types as X
import LordOfTheRings.Queue
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage QuestAttrs where
  runMessage msg attrs = case msg of
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      let attrs' = attrs & tokensL %~ addTokens token n
      done <- hasEnoughProgress attrs'
      when (token == Progress && done) $ push (CompleteQuest attrs.id)
      pure attrs'
    -- a stage that does its setup as it turns over
    PerformSetup (isTarget attrs -> True) -> do
      turnOver attrs
      pure attrs
    Flip (isTarget attrs -> True) -> pure $ attrs & flippedL .~ True
    -- a stage put into play unflipped is clicked by its card, before it has an id to show
    Flip (CardIdTarget cid) | cid == attrs.questCardId -> pure $ attrs & flippedL .~ True
    PlaceUnderneath (isTarget attrs -> True) cards -> pure $ attrs & underneathL <>~ cards
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    -- a stage with enough progress is defeated as it is checked, as once nothing stops it
    CheckStage qid | qid == attrs.id -> do
      done <- hasEnoughProgress attrs
      when done $ push (CompleteQuest attrs.id)
      pure attrs
    _ -> pure attrs

{- | The stage's quest points, its modifiers applied, as Haunting Fog's +6 and Thaurdir's
Pursuit's -5 for each Clue read.
-}
getQuestPoints :: HasGame m => QuestAttrs -> m Int
getQuestPoints attrs = modifiedValue questPointsOf attrs attrs.questPoints

-- | A stage with quest points printed has as much progress as it now has quest points.
hasEnoughProgress :: HasGame m => QuestAttrs -> m Bool
hasEnoughProgress attrs
  | attrs.questPoints > 0 = (attrs.progress >=) <$> getQuestPoints attrs
  | otherwise = pure False

-- | The lead clicks the first stage to perform its setup, which the stage handles as
-- 'PerformSetup' on itself before it turns over.
askToPerformSetup :: ReverseQueue m => QuestAttrs -> m ()
askToPerformSetup attrs = leadClicks "performSetup" attrs (PerformSetup $ toTarget attrs)

-- | The stage turns over to side B, once its side A setup has resolved.
turnOver :: ReverseQueue m => QuestAttrs -> m ()
turnOver attrs = push $ Flip (toTarget attrs)

{- | The lead clicks the stage to resolve its When Revealed effect, which the stage
handles as 'WhenRevealed' on itself.
-}
askToResolveWhenRevealed :: ReverseQueue m => QuestAttrs -> m ()
askToResolveWhenRevealed attrs = leadClicks "resolveWhenRevealed" attrs (WhenRevealed $ toTarget attrs)

leadClicks :: ReverseQueue m => Text -> QuestAttrs -> Message -> m ()
leadClicks key attrs msg = do
  lead <- getLead
  chooseOneM lead do
    questionLabeledWith key (cardVar "card" attrs)
    targeting attrs $ push msg

{- | A stage that cannot be defeated while an enemy matching the matcher is in play: its
progress piles up, and once the last such enemy leaves play a stage with enough
progress is defeated. Runs the message as an ordinary stage otherwise.
-}
runBlockedStage :: EnemyMatcher -> Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runBlockedStage blocker msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Progress n -> do
    blocked <- selectAny blocker
    if blocked then pure $ attrs & tokensL %~ addTokens Progress n else runMessage msg attrs
  _ | Just eid <- leaving -> do
    isBlocker <- selectAny (EnemyWithId eid <> blocker)
    -- the one leaving is still counted: no other blocker may stay behind
    blockers <- length <$> select blocker
    done <- hasEnoughProgress attrs
    when (isBlocker && blockers == 1 && done)
      $ push (CompleteQuest attrs.id)
    runMessage msg attrs
  CheckStage qid | qid == attrs.id -> do
    blocked <- selectAny blocker
    if blocked then pure attrs else runMessage msg attrs
  _ -> runMessage msg attrs
 where
  leaving = case msg of
    Destroy _ (EnemyTarget eid) -> Just eid
    Discard (EnemyTarget eid) -> Just eid
    ReturnToTopOfEncounterDeck (EnemyTarget eid) -> Just eid
    _ -> Nothing

{- | A stage the players cannot advance from until the condition holds: its progress piles
up, and it is defeated once it has enough progress and the condition is met. Messages
that may meet the condition have it check again once they have resolved.
-}
runConditionalStage
  :: QueueT Message GameT Bool -> Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runConditionalStage condition msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Progress n -> do
    met <- condition
    if met then runMessage msg attrs else pure $ attrs & tokensL %~ addTokens Progress n
  CheckStage qid | qid == attrs.id -> do
    met <- condition
    done <- hasEnoughProgress attrs
    when (met && done)
      $ push (CompleteQuest attrs.id)
    pure attrs
  ClaimObjective {} -> recheck
  RescuePrisoner {} -> recheck
  RescueCharacter {} -> recheck
  AddToVictoryDisplay {} -> recheck
  Discard (LocationTarget _) -> recheck
  _ -> runMessage msg attrs
 where
  recheck = do
    push $ CheckStage attrs.id
    runMessage msg attrs

-- | Objectives the players have claimed.
claimedObjectives :: HasGame m => m Int
claimedObjectives =
  getsGame (length . filter (isAttached . (.placement)) . toList . entitiesObjectives . gameEntities)
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False
