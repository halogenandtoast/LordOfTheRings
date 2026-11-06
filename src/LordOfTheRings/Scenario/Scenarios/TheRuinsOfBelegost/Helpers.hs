module LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers
  ( playerControlsLoot, lootControlledBy, stalkingResources, stalkingObjectives, placeStalkingResources, summonNaurlhug, controlledLootCount, inBelegostVictory, heroHasLoot ) where

import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs)
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootObjectives)
import LordOfTheRings.Target (toTarget)
import LordOfTheRings.Source (Sourceable)

playerControlsLoot :: HasGame m => PlayerId -> m Bool
playerControlsLoot pid = do
  notNull <$> lootControlledBy pid

lootControlledBy :: HasGame m => PlayerId -> m [ObjectiveAttrs]
lootControlledBy pid = do
  loot <- lootObjectives
  heroes <- select (HeroControlledBy $ PlayerWithId pid)
  pure $ filter (\o -> any (\h -> o.placement == AttachedTo (toTarget h)) heroes) loot

stalkingResources :: HasGame m => m Int
stalkingResources = sum . map (.resources) <$> stalkingObjectives

stalkingObjectives :: HasGame m => m [ObjectiveAttrs]
stalkingObjectives = getsGame \g ->
  [ toAttrs o
  | o <- toList g.entities.objectives
  , toCardCode o == toCardCode Objectives.stalkingTheRuins
  ]

placeStalkingResources :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeStalkingResources source n = traverse_ (\o -> placeResources source o n) =<< stalkingObjectives

summonNaurlhug :: ReverseQueue m => m ()
summonNaurlhug = do
  dragons <- select $ EnemyWithCardCode (toCardCode Enemies.naurlhug)
  if null dragons
    then addSetAsideToStaging (cardIs Enemies.naurlhug)
    else for_ dragons \eid -> placeEnemy eid StagingArea

controlledLootCount :: HasGame m => m Int
controlledLootCount = sum <$> (traverse (fmap length . lootControlledBy) =<< getPlayerOrder)

heroHasLoot :: HasGame m => CardDef -> m Bool
heroHasLoot def = do
  loot <- concatMapM lootControlledBy =<< getPlayerOrder
  pure $ any ((== toCardCode def) . toCardCode) loot

inBelegostVictory :: HasGame m => CardDef -> m Bool
inBelegostVictory def = getsGame $ any ((== toCardCode def) . toCardCode) . (.victoryDisplay) . toAttrs . (.scenario)
