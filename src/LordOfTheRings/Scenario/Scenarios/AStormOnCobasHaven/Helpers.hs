-- | What the cards of A Storm on Cobas Haven share: the Dol Amroth objectives, guarded by
-- the encounter cards attached to them, and in the players' control once free of them.
module LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (
  isGuarded,
  controlledByPlayers,
  guardedObjectives,
  unguardedObjectives,
  guardingCards,
  notGuarding,
  mayGuard,
  objectiveWithCardId,
  dolAmrothObjectivesToSearch,
  raiderFlagship,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (EncounterCard, IsCard (toCard), toCardCode, toCardDef, cdCardTraits)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesEnemies, entitiesLocations, entitiesObjectives, entitiesTreacheries)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Id (CardId, EnemyId, ObjectiveId)
import LordOfTheRings.Matcher
import LordOfTheRings.Objective.Types (Objective, ObjectiveAttrs (..))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck, scenarioEncounterDiscard)
import LordOfTheRings.Trait (Trait (DolAmroth))

raiderFlagship :: EnemyMatcher
raiderFlagship = EnemyWithCardCode (toCardCode Enemies.raiderFlagship)

-- | The cards in play that may guard an objective: enemies, locations and treacheries.
cardsInPlay :: HasGame m => m [CardId]
cardsInPlay = do
  es <- getsGame gameEntities
  pure
    $ map ((.id) . toCard) (toList $ entitiesEnemies es)
    <> map ((.id) . toCard) (toList $ entitiesLocations es)
    <> map ((.id) . toCard) (toList $ entitiesTreacheries es)

-- | An encounter card guarding the objective is still in play.
isGuarded :: HasGame m => ObjectiveAttrs -> m Bool
isGuarded o = maybe (pure False) (\cid -> elem cid <$> cardsInPlay) (objectiveGuard o)

-- | "If it is free of encounters, add it to the staging area, controlled by the players":
-- the players control it while nothing guards it.
controlledByPlayers :: HasGame m => ObjectiveAttrs -> m Bool
controlledByPlayers o = not <$> isGuarded o

objectives :: HasGame m => m [Objective]
objectives = getsGame (toList . entitiesObjectives . gameEntities)

guardedObjectives :: HasGame m => m [ObjectiveId]
guardedObjectives = map (.id) <$> (filterM (isGuarded . toAttrs) =<< objectives)

unguardedObjectives :: HasGame m => m [ObjectiveId]
unguardedObjectives =
  map (.id) <$> (filterM (fmap not . isGuarded . toAttrs) . filter ((== StagingArea) . (.placement)) =<< objectives)

-- | The cards guarding an objective.
guardingCards :: HasGame m => m [CardId]
guardingCards = mapMaybe (objectiveGuard . toAttrs) <$> (filterM (isGuarded . toAttrs) =<< objectives)

-- | The enemies among these that guard no objective.
notGuarding :: HasGame m => [EnemyId] -> m [EnemyId]
notGuarding eids = do
  guards <- guardingCards
  es <- getsGame (entitiesEnemies . gameEntities)
  pure [eid | eid <- eids, maybe True ((`notElem` guards) . (.id) . toCard) (Map.lookup eid es)]

-- | The enemies among these free to guard an objective: guarding none, and in nightmare
-- mode not Raider Flagship ("Raider Flagship cannot guard objectives").
mayGuard :: HasGame m => [EnemyId] -> m [EnemyId]
mayGuard eids = do
  nightmare <- inNightmareMode
  flagships <- if nightmare then select raiderFlagship else pure []
  filter (`notElem` flagships) <$> notGuarding eids

objectiveWithCardId :: HasGame m => CardId -> m (Maybe ObjectiveId)
objectiveWithCardId cid = fmap (.id) . find ((== cid) . objectiveCardId . toAttrs) <$> objectives

-- | The Dol Amroth objectives in the encounter deck and discard pile.
dolAmrothObjectivesToSearch :: HasGame m => m [EncounterCard]
dolAmrothObjectivesToSearch = do
  scenario <- getsGame (toAttrs . gameScenario)
  pure $ filter ((DolAmroth `member`) . cdCardTraits . toCardDef) (scenarioEncounterDeck scenario <> scenarioEncounterDiscard scenario)
