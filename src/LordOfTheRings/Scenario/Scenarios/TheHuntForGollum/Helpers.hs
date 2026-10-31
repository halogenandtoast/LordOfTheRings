-- | What The Hunt for Gollum's nightmare cards share: Clue cards attached to Mordor enemies.
module LordOfTheRings.Scenario.Scenarios.TheHuntForGollum.Helpers (
  mordorEnemies,
  cluesOnMordorEnemies,
  playersWithClues,
  attachClueToMordorEnemy,
  clueToMordorEnemy,
) where

import LordOfTheRings.Card (EncounterCard)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (CardId, EnemyId, ObjectiveId, PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.Types (objectiveGuard)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Targetable)
import LordOfTheRings.Trait (Trait (Clue, Mordor))

-- | The Mordor enemies in play, with the card each is.
mordorEnemies :: HasGame m => m [(EnemyId, CardId)]
mordorEnemies = traverse (\e -> (e,) . (.cardId) . toAttrs <$> getEnemy e) =<< select (EnemyWithTrait Mordor)

-- | The Clue cards attached to Mordor enemies: those a Mordor enemy guards.
cluesOnMordorEnemies :: HasGame m => m [ObjectiveId]
cluesOnMordorEnemies = do
  guards <- map snd <$> mordorEnemies
  clues <- objectivesWithTrait Clue
  pure [toId o | o <- clues, maybe False (`elem` guards) (objectiveGuard $ toAttrs o)]

-- | The players who control a Clue card: one attached to a hero of theirs.
playersWithClues :: HasGame m => m [PlayerId]
playersWithClues = filterM (\p -> selectAny (HeroControlledBy (PlayerWithId p) <> HeroWithAttached Clue)) =<< getRemainingPlayers

-- | The player attaches the Clue card in play to a Mordor enemy of his choice, which now
-- guards it; one attached to a hero leaves it for the staging area.
attachClueToMordorEnemy :: (ReverseQueue m, Targetable source) => source -> PlayerId -> ObjectiveId -> m ()
attachClueToMordorEnemy source pid oid = do
  enemies <- mordorEnemies
  chooseOrRunOneM pid do
    questionLabeled "theHuntForGollumNightmare.mordorEnemy"
    causedBy source
    for_ enemies \(e, cid) -> targeting e do
      placeObjective oid StagingArea
      guardObjectiveWith oid cid

-- | As 'attachClueToMordorEnemy', for a Clue card out of play, as one in the encounter
-- discard pile.
clueToMordorEnemy :: (ReverseQueue m, Targetable source) => source -> PlayerId -> EncounterCard -> m ()
clueToMordorEnemy source pid card = do
  enemies <- mordorEnemies
  chooseOrRunOneM pid do
    questionLabeled "theHuntForGollumNightmare.mordorEnemy"
    causedBy source
    for_ enemies \(e, cid) -> targeting e do
      removeFromEncounterDiscard card
      putObjectiveIntoPlayGuardedBy card cid
