module LordOfTheRings.Quest.Cards.Ambush (ambush) where

import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype Ambush = Ambush QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ambush :: QuestCard Ambush
ambush = quest Ambush Cards.ambush

-- | Forced: At the beginning of the combat phase, all enemies in play engage the player
-- guarding Gollum.
instance HasAbilities Ambush where
  getAbilities (Ambush a) = [restricted a 1 (exists AnyEnemy) $ forced $ PhaseBeginsWindow After CombatPhase]

-- | Players cannot defeat this stage if there are any enemies in play. If players defeat
-- this stage, they have won the game.
instance RunMessage Ambush where
  runMessage msg q@(Ambush attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getGuard >>= traverse_ \g -> traverse_ (engageEnemy g) =<< select AnyEnemy
      pure q
    _ -> Ambush <$> runBlockedStage AnyEnemy msg attrs
