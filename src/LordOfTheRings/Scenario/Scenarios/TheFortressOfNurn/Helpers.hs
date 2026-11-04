-- | What the cards of The Fortress of Nurn share: the Castle side quests, in play and in
-- the victory display, and the cards the players place under them.
module LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (
  castleSideQuests,
  castlesInVictoryDisplay,
  castleX,
  placeFromHandUnderCastleSideQuest,
  runStormTheCastle,
) where

import LordOfTheRings.Card (Card (EncounterCard, PlayerCard), cdCardTraits, toCard, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Matcher (getPlayer)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Castle))

isCastle :: Card -> Bool
isCastle = member Castle . cdCardTraits . toCardDef

-- | The Castle side quests in play, either side faceup.
castleSideQuests :: HasGame m => m [QuestId]
castleSideQuests =
  getsGame (map (.id) . filter (\q -> isSideQuest q && isCastle (toCard q)) . map toAttrs . toList . entitiesQuests . gameEntities)

castlesInVictoryDisplay :: HasGame m => m Int
castlesInVictoryDisplay = getsGame (length . filter isCastle . (.victoryDisplay) . toAttrs . (.scenario))

-- | "X is 1 more than the number of Castle side quests in the victory display."
castleX :: HasGame m => m Int
castleX = (+ 1) <$> castlesInVictoryDisplay

{- | "Place 1 card from your hand facedown under a Castle side quest": the player chooses
the card, then the side quest. Nothing with an empty hand or no Castle side quest in play.
-}
placeFromHandUnderCastleSideQuest
  :: (ReverseQueue m, Targetable source) => source -> PlayerId -> m ()
placeFromHandUnderCastleSideQuest source pid = do
  hand <- (.hand) <$> getPlayer pid
  castles <- castleSideQuests
  unless (null castles) $ chooseOneM pid do
    questionLabeled "theFortressOfNurn.cardFromHand"
    causedBy source
    targets hand \card -> chooseOrRunOneM pid do
      questionLabeled "theFortressOfNurn.castleSideQuest"
      causedBy source
      targets castles \q -> placeUnderneath q [card]

{- | A Storm the Castle side quest, its other side's When Revealed effect the card's own.
"The faceup encounter card underneath this quest is not in play. Forced: When this quest
becomes the current quest, reveal the faceup encounter card under it. Then, flip this card
over and make it the current quest until the end of the phase." Choosing it already made it
the current quest until the end of the phase, which it stays as it turns over; the rule
sheet has the players reveal the card and resolve its staging first, then flip it. Its
other side: "When this stage is defeated, shuffle each player card underneath it into its
owner's deck. Then, each player draws 1 card."
-}
runStormTheCastle :: Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runStormTheCastle msg attrs = case msg of
  SetCurrentQuest (isTarget attrs -> True) | not attrs.flipped -> do
    for_ [ec | EncounterCard ec <- attrs.underneath] (push . RevealCard)
    flipStage attrs
    runMessage msg $ attrs & underneathL %~ filter (\case EncounterCard _ -> False; _ -> True)
  Flip (isTarget attrs -> True) | not attrs.flipped -> do
    askToResolveWhenRevealed attrs
    runMessage msg attrs
  CompleteQuest qid | qid == attrs.id && attrs.flipped -> do
    for_ [(owner, pc) | PlayerCard pc <- attrs.underneath, Just owner <- [pc.owner]] \(owner, pc) ->
      shuffleIntoDeck owner (PlayerCard pc)
    eachPlayer \p -> drawCards attrs p 1
    runMessage msg attrs
  _ -> runMessage msg attrs

