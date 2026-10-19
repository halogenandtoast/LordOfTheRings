module LordOfTheRings.Helpers.Staging (module LordOfTheRings.Helpers.Staging) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Id
import LordOfTheRings.Target

getEnemyThreat :: (HasGame m) => EnemyId -> m Int
getEnemyThreat eid = do
  enemy <- getEnemy eid
  blank <- elem DoesNotContributeThreat <$> getModifiers eid
  if blank then pure 0 else modifiedValue threatOf (toTarget eid) enemy.threat

getLocationThreat :: (HasGame m) => LocationId -> m Int
getLocationThreat lid = do
  location <- getLocation lid
  blank <- elem DoesNotContributeThreat <$> getModifiers lid
  if blank then pure 0 else modifiedValue threatOf (toTarget lid) location.threat

-- | The total threat of every card in the staging area.
getStagingThreat :: (HasGame m) => m Int
getStagingThreat = do
  enemies <- traverse getEnemyThreat =<< select EnemyInStagingArea
  locations <- traverse getLocationThreat =<< select LocationInStagingArea
  bonus <- sum . mapMaybe (\case StagingThreatBonus n -> Just n; _ -> Nothing) <$> getModifiers GameTarget
  pure $ sum enemies + sum locations + bonus

getEngagementCost :: (HasGame m) => EnemyId -> m Int
getEngagementCost eid = do
  enemy <- getEnemy eid
  modifiedValue engagementCostOf (toTarget eid) enemy.engagementCost

getEnemyHitPoints :: (HasGame m) => EnemyId -> m Int
getEnemyHitPoints eid = do
  enemy <- getEnemy eid
  modifiedValue hitPointsOf (toTarget eid) enemy.hitPoints

getEnemyAttack :: (HasGame m) => EnemyId -> m Int
getEnemyAttack eid = do
  enemy <- getEnemy eid
  modifiedValue attackOf (toTarget eid) enemy.attack

getEnemyDefense :: (HasGame m) => EnemyId -> m Int
getEnemyDefense eid = do
  enemy <- getEnemy eid
  modifiedValue defenseOf (toTarget eid) enemy.defense
