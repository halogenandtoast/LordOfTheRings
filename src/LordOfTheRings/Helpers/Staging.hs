module LordOfTheRings.Helpers.Staging (module LordOfTheRings.Helpers.Staging) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Matcher
import LordOfTheRings.Prelude
import LordOfTheRings.Id
import LordOfTheRings.Target

getEnemyThreat :: (HasGame m) => EnemyId -> m Int
getEnemyThreat eid = do
  enemy <- getEnemy eid
  modifiedValue threatOf (toTarget eid) enemy.threat

getLocationThreat :: (HasGame m) => LocationId -> m Int
getLocationThreat lid = do
  location <- getLocation lid
  modifiedValue threatOf (toTarget lid) location.threat

-- | The total threat of every card in the staging area.
getStagingThreat :: (HasGame m) => m Int
getStagingThreat = do
  enemies <- traverse getEnemyThreat =<< select EnemyInStagingArea
  locations <- traverse getLocationThreat =<< select LocationInStagingArea
  pure $ sum enemies + sum locations
