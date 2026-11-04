-- | What Mount Gundabad's cards share: Dagnir, the quest cards in the victory display that
-- make her X, Wormsbane, and the Exploring Gundabad stages of the quest deck.
module LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers (
  questCardsInVictoryDisplay,
  dagnirX,
  getDagnir,
  returnDagnirToStaging,
  getWormsbane,
  wormsbaneHost,
  exploringGundabad,
  bypass,
  runExploringGundabad,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (CardType (QuestType), cdCardType, isSideQuestType, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Token (Token (Progress), addTokens)
import LordOfTheRings.Window (WindowType (QuestBypassed), mkWhen)

-- | The quest cards in the victory display: The Side Door, each stage defeated, and any side
-- quest, a quest card too.
questCardsInVictoryDisplay :: HasGame m => m Int
questCardsInVictoryDisplay =
  getsGame (length . filter (isQuestCard . cdCardType . toCardDef) . (.victoryDisplay) . toAttrs . (.scenario))
 where
  isQuestCard t = t == QuestType || isSideQuestType t

-- | "X is 1 more than the number of quest cards in the victory display."
dagnirX :: HasGame m => m Int
dagnirX = (+ 1) <$> questCardsInVictoryDisplay

getDagnir :: HasGame m => m (Maybe EnemyId)
getDagnir = selectOne (EnemyWithCardCode $ toCardCode Enemies.dagnir)

-- | "Return Dagnir to the staging area": nothing if she is there already.
returnDagnirToStaging :: ReverseQueue m => m ()
returnDagnirToStaging = do
  engaged <- select (EnemyWithCardCode (toCardCode Enemies.dagnir) <> NotEnemy EnemyInStagingArea)
  for_ engaged \e -> placeEnemy e StagingArea

-- | Wormsbane in play, whichever side is faceup.
getWormsbane :: HasGame m => m (Maybe ObjectiveId)
getWormsbane =
  getsGame
    $ fmap toId
    . find ((`elem` map toCardCode [Objectives.wormsbane, Objectives.wormsbaneReforged]) . toCardCode)
    . toList
    . entitiesObjectives
    . gameEntities

-- | The hero Wormsbane is attached to.
wormsbaneHost :: HasGame m => m (Maybe Target)
wormsbaneHost =
  getsGame
    $ headMay
    . mapMaybe (\o -> case (toAttrs o).placement of AttachedTo host -> Just host; _ -> Nothing)
    . filter ((`elem` map toCardCode [Objectives.wormsbane, Objectives.wormsbaneReforged]) . toCardCode)
    . toList
    . entitiesObjectives
    . gameEntities

{- | "If Exploring Gundabad is the current main quest, do not flip it over until the
beginning of the quest phase." The rule sheet says the staging step instead; Caleb ruled
the cards correct: "You should flip the quest cards over at the beginning of the quest
phase." (FFG forum, "Mount Gundabad: when to flip quest cards", April 2019.)
-}
exploringGundabad :: QuestAttrs -> [Ability]
exploringGundabad a = [mkAbility a 101 $ silent $ PhaseBeginsWindow When QuestPhase | not a.flipped]

-- | "At the end of the combat phase, the players may bypass this stage."
bypass :: QuestAttrs -> [Ability]
bypass a = [mkAbility a 102 $ response $ PhaseEndsWindow When CombatPhase | a.flipped]

{- | An Exploring Gundabad stage: side A takes no progress and flips itself at the
beginning of the quest phase. Defeated, its "When this stage is defeated" effect resolves
before it goes to the victory display and the next stage is revealed. Bypassed, it loses
its progress and goes to the bottom of the quest deck side A faceup, and the next stage 2A
is revealed (rule sheet).
-}
runExploringGundabad :: QueueT Message GameT () -> Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runExploringGundabad whenDefeated msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Progress n
    | not attrs.flipped -> pure attrs
    | otherwise -> do
        let attrs' = attrs & tokensL %~ addTokens Progress n
        defeatIfDone attrs'
        pure attrs'
  CheckStage qid | qid == attrs.id -> do
    defeatIfDone attrs
    pure attrs
  UseCardAbility _ (isSource attrs -> True) 101 _ -> do
    flipStage attrs
    pure attrs
  UseCardAbility _ (isSource attrs -> True) 102 _ -> do
    push $ CheckWindows [mkWhen $ QuestBypassed (toTarget attrs)]
    putOnBottomOfQuestDeck attrs
    advanceQuest
    pure attrs
  _ -> runMessage msg attrs
 where
  defeatIfDone a = whenM (hasEnoughProgress a) do
    whenDefeated
    completeQuest a.id
