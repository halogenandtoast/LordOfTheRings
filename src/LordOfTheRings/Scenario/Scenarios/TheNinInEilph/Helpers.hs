-- | What the cards of The Nîn-in-Eilph share: the stages the players wander among at
-- random, and Ancient Marsh-dweller, which the stages call back to the staging area.
module LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (
  stageOf,
  wanderingStage,
  runWanderingStage,
  advanceToDifferentStage,
  getMarshDweller,
  addMarshDweller,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (CardDef (cdStage), HasCardDef (toCardDef), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target

-- | The stage number printed on the quest card.
stageOf :: HasCardDef a => a -> Int
stageOf = fromMaybe 1 . cdStage . toCardDef

{- | Forced: After the last time counter is removed from this stage, advance to a different
stage of its number at random. Return this stage to the quest deck.
-}
wanderingStage :: QuestAttrs -> [Ability]
wanderingStage a = [mkAbility a 101 $ forced $ lastTimeCounterRemoved a]

{- | A stage 2 or 3: the lead clicks it to resolve its When Revealed, which the stage reads
as 'WhenRevealed', and it wanders to another as its last time counter goes.
-}
runWanderingStage :: Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runWanderingStage msg attrs = case msg of
  Flip (isTarget attrs -> True) | not attrs.flipped -> do
    askToResolveWhenRevealed attrs
    runMessage msg attrs
  UseCardAbility _ (isSource attrs -> True) 101 _ -> do
    advanceToDifferentStage attrs
    pure attrs
  _ -> runMessage msg attrs

{- | One of the other stages of the same number in the quest deck, at random, replaces this
one, which goes back to the quest deck with its progress lost.
-}
advanceToDifferentStage :: ReverseQueue m => QuestAttrs -> m ()
advanceToDifferentStage attrs = do
  deck <- getsGame ((.questDeck) . toAttrs . (.scenario))
  case filter ((== attrs.stage) . stageOf) deck of
    [] -> pure ()
    others -> do
      next <- uniform others
      shuffleIntoQuestDeck attrs
      advanceToQuest next

-- | Ancient Marsh-dweller, in play.
getMarshDweller :: HasGame m => m (Maybe EnemyId)
getMarshDweller = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.ancientMarshDweller)

{- | Add Ancient Marsh-dweller to the staging area, from out of play or engaged with a
player, and heal all damage from it: the scenario knows where it is.
-}
addMarshDweller :: ReverseQueue m => m ()
addMarshDweller = push $ ScenarioSpecific "marshDweller.add" Null
