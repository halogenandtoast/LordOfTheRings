{- | What The Battle of Five Armies' cards share: stages 2, 3 and 4 in play at the same time,
the first player choosing one as the current quest each round (The Hobbit: On the
Doorstep rules insert, "Multiple Stages in Play" and "Current Quest Stage").
-}
module LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (
  battleStages,
  stagesWithoutProgress,
  chosenStage,
  runBattleStage,
  removeProgressFromStage,
  stageTravelCost,
) where

import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Matcher (getMainQuests)
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEffects, gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CurrentQuest))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target
import LordOfTheRings.Token

-- | Stages 2, 3 and 4, in play at the same time.
battleStages :: [CardDef]
battleStages = [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill]

-- | The quest stages in play with no progress on them, as "for each quest stage in play
-- with no progress on it" reads.
stagesWithoutProgress :: HasGame m => m Int
stagesWithoutProgress =
  getsGame (length . filter (\q -> not (isSideQuest q) && q.progress == 0) . toList . entitiesQuests . gameEntities)

{- | The stage the first player chose as the current quest this round, read off the lasting
effect that makes it so, as a stage's modifiers may ask while the game's are gathered.
-}
chosenStage :: HasGame m => m (Maybe Target)
chosenStage = getsGame (headMay . map effectTarget . filter ((CurrentQuest `elem`) . effectModifiers) . gameEffects)

{- | Stages 2 to 4 each read "Players cannot advance from this stage unless" the other two
"are complete": a stage's progress piles up past its quest points, and once the last of
the three has as much as its quest points the players advance from all three, to stage 5.
-}
runBattleStage :: Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runBattleStage msg attrs = case msg of
  PlaceTokens _ (isTarget attrs -> True) Progress n -> do
    let attrs' = attrs & tokensL %~ addTokens Progress n
    done <- hasEnoughProgress attrs'
    others <- getsGame (filter (\q -> toCardCode q `elem` map toCardCode battleStages && toId q /= attrs.id) . toList . entitiesQuests . gameEntities)
    othersDone <- allM (hasEnoughProgress . toAttrs) others
    when (done && length others == 2 && othersDone) advanceQuest
    pure attrs'
  _ -> runMessage msg attrs

-- | Remove up to this much progress from the stage, as "remove 2 progress from stage 3B,
-- if able" reads.
removeProgressFromStage :: (ReverseQueue m, Sourceable source) => source -> CardDef -> Int -> m ()
removeProgressFromStage source def n = do
  quests <- getsGame (filter ((== toCardCode def) . toCardCode) . toList . entitiesQuests . gameEntities)
  for_ quests \q -> when (q.progress > 0) $ removeTokens source q Progress (min n q.progress)

{- | Ruins of Dale, Ravenhill and The Front Gate: "Spend 3 Baggins resources to travel here.
(1 Baggins resource instead if" their stage "is the current quest.)" The current quest is
read off the effect that chose it, as a location's modifiers ask for the cost.
-}
stageTravelCost :: HasGame m => CardDef -> m Int
stageTravelCost def = do
  chosen <- chosenStage
  mains <- getMainQuests
  quests <- getsGame (filter ((== toCardCode def) . toCardCode) . toList . entitiesQuests . gameEntities)
  let current = chosen <|> headMay mains
  pure $ if any ((== current) . Just . toTarget) quests then 1 else 3
