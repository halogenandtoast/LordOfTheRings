module LordOfTheRings.Quest.Runner (module LordOfTheRings.Quest.Runner, module X) where

import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.I18n
import LordOfTheRings.Question
import LordOfTheRings.Matcher
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Quest.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage QuestAttrs where
  runMessage msg attrs = case msg of
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      let attrs' = attrs & tokensL %~ addTokens token n
      when (token == Progress && attrs'.questPoints > 0 && attrs'.progress >= attrs'.questPoints)
        $ push (CompleteQuest attrs.id)
      pure attrs'
    Flip (isTarget attrs -> True) -> pure $ attrs & flippedL .~ True
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs

-- | The lead clicks the stage to resolve its When Revealed effect, which the stage
-- handles as 'WhenRevealed' on itself.
whenRevealedQuestion :: (HasGame m) => QuestAttrs -> m Message
whenRevealedQuestion attrs = do
  lead <- getLead
  pure $ questionLabel (ikeyWith "label.resolveWhenRevealed" (cardVar "quest" attrs)) lead [targetLabel attrs [WhenRevealed $ toTarget attrs]]

-- | A stage that cannot be defeated while an enemy matching the matcher is in play: its
-- progress piles up, and once the last such enemy leaves play a stage with enough
-- progress is defeated. Runs the message as an ordinary stage otherwise.
runBlockedStage :: EnemyMatcher -> Message -> QuestAttrs -> GameT QuestAttrs
runBlockedStage blocker msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Progress n -> do
    blocked <- selectAny blocker
    if blocked then pure $ attrs & tokensL %~ addTokens Progress n else runMessage msg attrs
  _ | Just eid <- leaving -> do
    isBlocker <- selectAny (EnemyWithId eid <> blocker)
    -- the one leaving is still counted: no other blocker may stay behind
    blockers <- length <$> select blocker
    when (isBlocker && blockers == 1 && attrs.questPoints > 0 && attrs.progress >= attrs.questPoints)
      $ push (CompleteQuest attrs.id)
    runMessage msg attrs
  _ -> runMessage msg attrs
 where
  leaving = case msg of
    Destroy (EnemyTarget eid) -> Just eid
    Discard (EnemyTarget eid) -> Just eid
    ReturnToTopOfEncounterDeck (EnemyTarget eid) -> Just eid
    _ -> Nothing

-- | A stage the players cannot advance from until the condition holds: its progress piles
-- up, and it is defeated once it has enough progress and the condition is met. Messages
-- that may meet the condition have it check again once they have resolved.
runConditionalStage :: GameT Bool -> Message -> QuestAttrs -> GameT QuestAttrs
runConditionalStage condition msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Progress n -> do
    met <- condition
    if met then runMessage msg attrs else pure $ attrs & tokensL %~ addTokens Progress n
  CheckStage qid | qid == attrs.id -> do
    met <- condition
    when (met && attrs.questPoints > 0 && attrs.progress >= attrs.questPoints) $ push (CompleteQuest attrs.id)
    pure attrs
  ClaimObjective {} -> recheck
  RescuePrisoner {} -> recheck
  _ -> runMessage msg attrs
 where
  recheck = do
    push $ CheckStage attrs.id
    runMessage msg attrs

-- | Objectives the players have claimed.
claimedObjectives :: (HasGame m) => m Int
claimedObjectives = getsGame (length . filter (isAttached . (.placement)) . toList . entitiesObjectives . gameEntities)
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False
