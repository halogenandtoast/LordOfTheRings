-- | What The Temple of Doom's cards share: the quest stages in the victory display that
-- make their X, Tribute to Mordor's damage tokens, and the stage 2 cards of the quest deck.
module LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers (
  questStagesInVictoryDisplay,
  templeX,
  placeDamageOnTribute,
  stage2Modifiers,
  stage2Forced,
  runStage2,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (CardType (QuestType), cdCardType, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (ProgressLimit))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Token (Token (Damage))

{- | "Quest stage refers only to the double-sided quest cards that come from the quest deck.
Therefore, when calculating the number of quest stages in the victory display count each
individual double-sided quest card found there, but do not count side quests in the
victory display." (MEC77 rules, Quest Stages vs. Side Quests)
-}
questStagesInVictoryDisplay :: HasGame m => m Int
questStagesInVictoryDisplay =
  getsGame (length . filter ((== QuestType) . cdCardType . toCardDef) . (.victoryDisplay) . toAttrs . (.scenario))

-- | "X is 1 more than the number of quest stages in the victory display."
templeX :: HasGame m => m Int
templeX = (+ 1) <$> questStagesInVictoryDisplay

-- | "Place 1 damage token on Tribute to Mordor": nothing once it has left play.
placeDamageOnTribute :: (ReverseQueue m, Sourceable source) => source -> m ()
placeDamageOnTribute source = do
  tributes <- getsGame (filter ((== toCardCode Quests.tributeToMordor) . toCardCode) . toList . entitiesQuests . gameEntities)
  for_ tributes \t -> placeTokens source (QuestTarget $ toId t) Damage 1

-- | "While there are 4 quest stages in the victory display, progress cannot be placed here."
stage2Modifiers :: HasModifiersM m => QuestAttrs -> m ()
stage2Modifiers a = when a.flipped do
  stages <- questStagesInVictoryDisplay
  when (stages >= 4) $ modifySelf a [ProgressLimit 0]

-- | Each stage 2B's "Forced: At the beginning of the quest phase, ..."
stage2Forced :: QuestAttrs -> [Ability]
stage2Forced a = [mkAbility a 1 $ forced $ PhaseBeginsWindow When QuestPhase | a.flipped]

{- | A stage 2A, revealed as the topmost card of the quest deck: "When Revealed: Reveal 1
encounter card per player." It turns over to side B as it is put into play.
-}
runStage2 :: Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runStage2 msg attrs = case msg of
  Flip (isTarget attrs -> True) | not attrs.flipped -> do
    askToResolveWhenRevealed attrs
    runMessage msg attrs
  WhenRevealed (isTarget attrs -> True) -> do
    eachPlayer $ const revealEncounterCard
    pure attrs
  _ -> runMessage msg attrs
