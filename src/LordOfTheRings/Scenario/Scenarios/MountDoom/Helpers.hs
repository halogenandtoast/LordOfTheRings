-- | What Mount Doom's cards share: the first treachery revealed each quest phase, as stages
-- 1B and 3B read it, and the Ring-bearer completing his quest.
module LordOfTheRings.Scenario.Scenarios.MountDoom.Helpers (
  firstTreacheryImmune,
  firstTreacherySurges,
  ringBearerCompletesQuest,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Campaign (CampaignLogKey (CampaignLost), CampaignLogUpdate (Record))
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Game.Base (gameEffects, gamePhase)
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Matcher (getLead)
import LordOfTheRings.Message.Lifted
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

{- | The Ring-bearer completes his quest and the players win the game. In campaign mode,
Mount Doom's campaign card resolves first, as the first player must choose: keep The One
Ring and succumb to the power of Sauron, the players losing the campaign, or destroy it,
the players winning the campaign.
-}
ringBearerCompletesQuest :: ReverseQueue m => m ()
ringBearerCompletesQuest = do
  inCampaign <- isJust <$> getCampaignLog
  when inCampaign do
    lead <- getLead
    chooseOneM lead do
      questionLabeled "mountDoom.resolution"
      labeled "mountDoom.keep" $ updateCampaignLog (Record CampaignLost [])
      labeled "mountDoom.destroy" $ pure ()
  winGame
