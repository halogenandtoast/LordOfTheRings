-- | What Mount Doom's cards share: the first treachery revealed each quest phase, as stages
-- 1B and 3B read it.
module LordOfTheRings.Scenario.Scenarios.MountDoom.Helpers (
  firstTreacheryImmune,
  firstTreacherySurges,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Game.Base (gameEffects, gamePhase)
import LordOfTheRings.Modifier (ModifierType (ImmuneToPlayerCardEffects, TreacheryRevealedThisPhase))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Window

{- | "The first treachery revealed each quest phase is immune to player card effects": the
first of the phase's revealed-treachery effects is the oldest.
-}
firstTreacheryImmune :: (HasModifiersM m, Sourceable source) => source -> m ()
firstTreacheryImmune source = do
  phase <- getsGame gamePhase
  effects <- getsGame gameEffects
  let revealed = [tid | Effect (TreacherySource tid) GameTarget ms _ <- effects, TreacheryRevealedThisPhase `elem` ms]
  when (phase == QuestPhase) $ for_ (lastMay revealed) \tid -> modified source (TreacheryTarget tid) [ImmuneToPlayerCardEffects]

-- | "... and gains surge": once it is revealed, the next card is.
firstTreacherySurges :: Sourceable a => a -> Int -> Ability
firstTreacherySurges a idx = limited (GroupLimit PerPhase 1) $ restricted a idx (DuringPhase QuestPhase) $ silent $ TreacheryRevealedWindow After
