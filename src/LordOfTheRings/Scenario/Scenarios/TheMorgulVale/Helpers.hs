-- | What the cards of The Morgul Vale share: the progress on To the Tower, and the stages
-- each held by a Captain.
module LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (
  getToTheTower,
  towerProgress,
  placeOnTower,
  removeFromTower,
  captainCannotLeavePlay,
  isCaptain,
) where

import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Id (EnemyId, ObjectiveId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CannotLeavePlay))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (Objective, objectiveTokens)
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Token

-- | To the Tower, in the staging area.
getToTheTower :: HasGame m => m (Maybe ObjectiveId)
getToTheTower = fmap toId <$> tower

tower :: HasGame m => m (Maybe Objective)
tower = getsGame $ find ((== toCardCode Objectives.toTheTower) . toCardCode) . toList . (.entities.objectives)

-- | The progress tokens on To the Tower.
towerProgress :: HasGame m => m Int
towerProgress = maybe 0 (countTokens Progress . objectiveTokens . toAttrs) <$> tower

placeOnTower :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeOnTower source n = getToTheTower >>= traverse_ \o -> placeProgress source o n

-- | Remove up to N progress tokens from To the Tower.
removeFromTower :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
removeFromTower source n = do
  progress <- towerProgress
  getToTheTower >>= traverse_ \o -> when (progress > 0) $ removeTokens source o Progress (min n progress)

-- | "<Captain> cannot leave play unless destroyed."
captainCannotLeavePlay :: (HasModifiersM m, Sourceable source) => source -> CardDef -> m ()
captainCannotLeavePlay source captain = do
  enemies <- select (EnemyWithCardCode $ toCardCode captain)
  for_ enemies \e -> modified source e [CannotLeavePlay]

-- | Whether the enemy, still in play, is the Captain.
isCaptain :: HasGame m => CardDef -> EnemyId -> m Bool
isCaptain captain eid = selectAny (EnemyWithId eid <> EnemyWithCardCode (toCardCode captain))
