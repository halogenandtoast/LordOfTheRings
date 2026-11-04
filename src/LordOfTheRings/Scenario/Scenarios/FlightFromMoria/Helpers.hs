-- | What Flight from Moria's cards share: The Nameless Fear, and the Search for an Exit
-- stages that wait face down until the staging step.
module LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers (
  namelessFear,
  namelessFearAttack,
  namelessFearThreat,
  searchForAnExit,
  bypass,
  runSearchForAnExit,
  shuffleBackFromVictoryDisplay,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Modifier (ModifierType (BypassCostRaisesThreat))
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Window (WindowType (QuestBypassed), mkWhen)
import LordOfTheRings.Token (Token (Progress))

-- | The Nameless Fear, while it is in play.
namelessFear :: HasGame m => m (Maybe EnemyId)
namelessFear = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.theNamelessFear)

-- | The Nameless Fear's attack, nothing while it is not in play.
namelessFearAttack :: HasGame m => m Int
namelessFearAttack = maybe (pure 0) getEnemyAttack =<< namelessFear

-- | The Nameless Fear's threat, nothing while it is not in play.
namelessFearThreat :: HasGame m => m Int
namelessFearThreat = maybe (pure 0) getEnemyThreat =<< namelessFear

{- | While Search for an Exit is the active quest card, only flip it to side 2B at the
beginning of the staging step.
-}
searchForAnExit :: QuestAttrs -> [Ability]
searchForAnExit a = [mkAbility a 101 $ silent StagingStepBeginsWindow | not a.flipped]

-- | Players may bypass this quest card at the end of the combat phase.
bypass :: QuestAttrs -> [Ability]
bypass a = [mkAbility a 102 $ response $ PhaseEndsWindow When CombatPhase | a.flipped]

{- | A Search for an Exit stage: side A takes no progress, it flips itself at the staging
step, and bypassing it puts it on the bottom of the quest deck for the next, opening the
window for what answers a bypass.
-}
runSearchForAnExit :: Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runSearchForAnExit msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Progress _ | not attrs.flipped -> pure attrs
  UseCardAbility _ (isSource attrs -> True) 101 _ -> do
    flipStage attrs
    pure attrs
  -- Nightmare: "When a quest card is bypassed, shuffle it into the quest deck instead of
  -- placing it on the bottom."
  UseCardAbility _ (isSource attrs -> True) 102 _ -> do
    -- the additional cost to bypass it is paid first, as Shadowed Corridor's
    raise <- sum . map (\case BypassCostRaisesThreat n -> n; _ -> 0) <$> getModifiers GameTarget
    when (raise > 0) $ eachPlayer \p -> raiseThreat p raise
    push $ CheckWindows [mkWhen $ QuestBypassed (toTarget attrs)]
    nightmare <- inNightmareMode
    if nightmare then shuffleIntoQuestDeck attrs else putOnBottomOfQuestDeck attrs
    advanceQuest
    pure attrs
  _ -> runMessage msg attrs

-- | Shuffle the stage back into the quest deck from the victory display, if it is there.
shuffleBackFromVictoryDisplay :: ReverseQueue m => CardDef -> m ()
shuffleBackFromVictoryDisplay def = do
  cards <- getsGame (filter (cardMatches $ cardIs def) . (.victoryDisplay) . toAttrs . (.scenario))
  for_ cards shuffleIntoQuestDeck
