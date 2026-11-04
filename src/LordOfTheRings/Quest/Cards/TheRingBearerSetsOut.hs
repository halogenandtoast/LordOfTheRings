module LordOfTheRings.Quest.Cards.TheRingBearerSetsOut (theRingBearerSetsOut) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheRingBearerSetsOut = TheRingBearerSetsOut QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theRingBearerSetsOut :: QuestCard TheRingBearerSetsOut
theRingBearerSetsOut = quest TheRingBearerSetsOut Cards.theRingBearerSetsOut

-- | During the quest phase, reveal 1 additional encounter card for each player in the game.
instance HasModifiersFor TheRingBearerSetsOut where
  getModifiersFor (TheRingBearerSetsOut a) = when a.flipped do
    phase <- getsGame gamePhase
    players <- length <$> getRemainingPlayers
    when (phase == QuestPhase) $ modified a GameTarget [AdditionalStagingReveals players]

-- | Response: After an enemy is revealed at this stage, raise your threat by X to engage that
-- enemy. X is that enemy's threat. Each player (at any stage) may trigger this ability once
-- per round.
instance HasAbilities TheRingBearerSetsOut where
  getAbilities (TheRingBearerSetsOut a) =
    [limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ response $ EnemyRevealedWindow After | a.flipped]

{- | When Revealed: Add Parth Galen to the staging area. If it is your quest phase, end the
phase (do not resolve the quest): your stage's, the other stages questing still.

When this stage is defeated, the players win the game: there is no stage after it.
-}
instance RunMessage TheRingBearerSetsOut where
  runMessage msg q@(TheRingBearerSetsOut attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheRingBearerSetsOut <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.parthGalen)
      phase <- getsGame gamePhase
      when (phase == QuestPhase) endQuestPhaseWithoutResolving
      pure q
    UseCardAbility pid (isSource attrs -> True) 1 ws -> do
      for_ [eid | Window _ (Revealed (EnemyTarget eid)) <- ws] \eid -> do
        x <- getEnemyThreat eid
        raiseThreat pid x
        engageEnemy pid eid
      pure q
    _ -> TheRingBearerSetsOut <$> runMessage msg attrs
