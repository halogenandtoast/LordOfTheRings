module LordOfTheRings.Quest.Cards.TheRearGuard (theRearGuard) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers

newtype TheRearGuard = TheRearGuard QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRearGuard :: QuestCard TheRearGuard
theRearGuard = quest TheRearGuard Cards.theRearGuard

-- | Forced: If at least 1 hero committed to the quest this round, place 4 progress tokens
-- on The Rear Guard (bypassing any active location) at the end of the quest phase.
-- If Durin's Bane has 0 hit points, advance to the next stage of the scenario immediately.
instance RunMessage TheRearGuard where
  runMessage msg q@(TheRearGuard attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      checkStage attrs.id
      TheRearGuard <$> runMessage msg attrs
    -- read as the phase ends, before the heroes leave the quest
    -- ponytail: a hero removed from the quest earlier in the phase is not counted
    EndPhase QuestPhase -> do
      committed <- selectAny CommittedHero
      when committed $ placeProgress attrs attrs 4
      TheRearGuard <$> runMessage msg attrs
    PlaceTokens _ (EnemyTarget _) Damage _ -> do
      checkStage attrs.id
      TheRearGuard <$> runMessage msg attrs
    CheckStage qid | qid == attrs.id -> do
      bane <- getDurinsBane
      for_ bane \eid -> do
        left <- durinsBaneHitPointsLeft eid
        when (left <= 0) $ push (CompleteQuest attrs.id)
      pure q
    _ -> TheRearGuard <$> runMessage msg attrs
