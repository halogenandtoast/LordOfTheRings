module LordOfTheRings.Helpers.Staging (module LordOfTheRings.Helpers.Staging) where

import LordOfTheRings.Card (IsCard (..))
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Id
import LordOfTheRings.Placement (Placement (OrcsArea))
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
  cards <- getStagingThreats
  gameMods <- getModifiers GameTarget
  let bonus = sum $ mapMaybe (\case StagingThreatBonus n -> Just n; _ -> Nothing) gameMods
  -- the cards in the Orcs' area, apart from the players' staging area, add theirs as they
  -- are now, as Howling at their Heels has them (Race Across Harad)
  orcs <-
    if OrcsAreaThreatAddsToStaging `elem` gameMods
      then do
        enemies <- getsGame (map toId . filter ((== OrcsArea) . (.placement)) . toList . (.entities.enemies))
        locations <- getsGame (map toId . filter ((== OrcsArea) . (.placement)) . toList . (.entities.locations))
        (+) <$> (sum <$> traverse getEnemyThreat enemies) <*> (sum <$> traverse getLocationThreat locations)
      else pure 0
  -- a reduction, as Followed's, takes it no lower than 0
  pure $ max 0 (sum (map snd cards) + bonus + orcs)

-- | What each card in the staging area adds to its threat, modifiers included.
getStagingThreats :: (HasGame m) => m [(CardId, Int)]
getStagingThreats = do
  enemies <- for' EnemyInStagingArea getEnemy getEnemyThreat
  matchers <- mapMaybe (\case EngagedEnemiesAddThreat m -> Just m; _ -> Nothing) <$> getModifiers GameTarget
  engaged <- concatMapM (\m -> for' (EnemyEngagedWith AnyPlayer <> m) getEnemy getEnemyThreat) matchers
  locations <- for' LocationInStagingArea getLocation getLocationThreat
  -- an active location adding its threat, as Flooded Ruins reads
  activeMatchers <- mapMaybe (\case ActiveLocationsAddThreat m -> Just m; _ -> Nothing) <$> getModifiers GameTarget
  actives <- concatMapM (\m -> for' (IsActiveLocation <> m) getLocation getLocationThreat) activeMatchers
  -- an enemy engaged and considered in the staging area too, as Tide-runner, adds it once
  pure $ ordNub (enemies <> engaged) <> locations <> ordNub actives
 where
  for' matcher get threat = select matcher >>= traverse \i -> (,) <$> ((.id) . toCard <$> get i) <*> threat i

-- | The location's quest points, modifiers included.
getLocationQuestPoints :: (HasGame m) => LocationId -> m Int
getLocationQuestPoints lid = do
  location <- getLocation lid
  modifiedValue questPointsOf (toTarget lid) location.questPoints

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
  -- one whose defense cannot be reduced, as Rupert, ignores what would
  floored <- elem CannotHaveDefenseReduced <$> getModifiers eid
  modifiedValue (if floored then \m -> defenseOf m >>= \n -> n <$ guard (n > 0) else defenseOf) (toTarget eid) enemy.defense
