-- | What the cards of Escape from Umbar share: the stages' cap and block, and the main quest
-- in play since the round began, which the scenario keeps in its meta.
module LordOfTheRings.Scenario.Scenarios.EscapeFromUmbar.Helpers (
  fleeing,
  mainQuestInPlayAllRound,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (CannotBeDefeated, ProgressLimit))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioMeta))
import LordOfTheRings.Target

{- | Both stages' B sides: "This stage cannot have more than 15 progress tokens on it. While
at least 1 player is engaged with an enemy, the players cannot defeat this stage."
-}
fleeing :: HasModifiersM m => QuestAttrs -> m ()
fleeing a = when a.flipped do
  modifySelf a [ProgressLimit (max 0 (15 - a.progress))]
  engaged <- selectAny (EnemyEngagedWith AnyPlayer)
  when engaged $ modifySelf a [CannotBeDefeated]

{- | Whether the main quest in play now was in play as the round began, as Seize Them!'s
"if the main quest has been in play the entire round" reads. The scenario notes the main
quest at each BeginRound.
-}
mainQuestInPlayAllRound :: HasGame m => m Bool
mainQuestInPlayAllRound = do
  atStart <- getsGame (parseMaybe parseJSON . scenarioMeta . toAttrs . (.scenario))
  now <- getMainQuests
  pure $ notNull now && atStart == Just (now :: [Target])
