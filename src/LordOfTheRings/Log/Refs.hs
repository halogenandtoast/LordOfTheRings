-- | Log refs that need the game to look something up, as in Arkham's "Arkham.Log.Refs".
--
-- A message carries ids; these turn them into chips. Nothing here throws: the narrator
-- runs inside the message loop, and an id routinely names something already gone.
module LordOfTheRings.Log.Refs (module LordOfTheRings.Log.Refs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base
import LordOfTheRings.Hero.Types (HeroAttrs (..))
import LordOfTheRings.Id
import LordOfTheRings.Log
import LordOfTheRings.Log.Block
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

playerRefFor :: (HasGame m) => PlayerId -> m LogRef
playerRefFor pid = do
  players <- getsGame gamePlayers
  pure $ playerRef pid $ maybe (idText pid) (.name) (Map.lookup pid players)

-- | A chip for what a target points at, when it is something a reader would recognise.
targetRefFor :: (HasGame m) => Target -> m (Maybe LogRef)
targetRefFor target = do
  g <- getGame
  let es = g.entities
      ref kind m k = entityRef kind k <$> Map.lookup k m
  case target of
    HeroTarget k -> pure $ (\h -> (entityRef RefHero k h) {logRefFaceDown = heroPrisoner (toAttrs h)}) <$> Map.lookup k es.heroes
    -- one that has just left play is still named, as its own ability answers it
    AllyTarget k -> pure $ ref RefAlly es.allies k <|> ref RefAlly (gameLeftPlay g) k
    EnemyTarget k -> pure $ ref RefEnemy es.enemies k
    LocationTarget k -> pure $ ref RefLocation es.locations k
    AttachmentTarget k -> pure $ ref RefAttachment es.attachments k
    TreacheryTarget k -> pure $ ref RefTreachery es.treacheries k
    ObjectiveTarget k -> pure $ ref RefObjective es.objectives k
    QuestTarget k -> pure $ (\q -> (entityRef RefQuest k q) {logRefFaceDown = q.flipped}) <$> Map.lookup k es.quests
    PlayerTarget pid -> Just <$> playerRefFor pid
    CardIdTarget cid -> pure $ cardRef <$> findCard cid g
    ScenarioTarget -> pure Nothing
    GameTarget -> pure Nothing

sourceRefFor :: (HasGame m) => Source -> m (Maybe LogRef)
sourceRefFor = \case
  PlayerSource pid -> Just <$> playerRefFor pid
  source -> maybe (pure Nothing) targetRefFor (sourceTarget source)

-- | A card by id wherever it is not in play: a hand, a deck, a discard pile, the
-- encounter deck, the staging reveal, or under an enemy as a shadow card.
findCard :: CardId -> Game -> Maybe Card
findCard cid g = find ((== cid) . (.id)) everywhere
 where
  players = toList g.players
  scenario = toAttrs g.scenario
  everywhere =
    concatMap (\p -> p.hand <> p.deck <> p.discard) players
      <> map EncounterCard (scenario.encounterDeck <> scenario.encounterDiscard)
      <> map EncounterCard (concatMap (.shadowCards) (toList g.entities.enemies))

-- | Send an entry, filed under whatever block is open, and stamped with where in the game
-- it happened. An entry that already names its group keeps it: a block's own header and
-- summary set theirs.
sendLogInOpenBlock :: (HasGame m, Frontend m) => LogEntry -> m ()
sendLogInOpenBlock entry = do
  g <- getGame
  let context = LogContext g.round (phaseKey g.phase)
      grouped = case (entry.logEntryGroup, logBlockPath g.gameLogBlocks) of
        (Nothing, Just path) -> inGroupOf path entry
        _ -> entry
  sendLog grouped {logEntryContext = Just context}

-- | The wire name of a phase, matching @log.phase.*@.
phaseKey :: Phase -> Text
phaseKey = \case
  ResourcePhase -> "resource"
  PlanningPhase -> "planning"
  QuestPhase -> "quest"
  TravelPhase -> "travel"
  EncounterPhase -> "encounter"
  CombatPhase -> "combat"
  RefreshPhase -> "refresh"
