module LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers
  ( playerControlsLoot, stalkingResources, summonNaurlhug ) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootObjectives)
import LordOfTheRings.Target (toTarget)

playerControlsLoot :: HasGame m => PlayerId -> m Bool
playerControlsLoot pid = do
  loot <- lootObjectives
  heroes <- select (HeroControlledBy $ PlayerWithId pid)
  pure $ any (\o -> any (\h -> o.placement == AttachedTo (toTarget h)) heroes) loot

stalkingResources :: HasGame m => m Int
stalkingResources = getsGame \g -> sum
  [ (toAttrs o).resources
  | o <- toList g.entities.objectives
  , toCardCode o == toCardCode Objectives.stalkingTheRuins
  ]

summonNaurlhug :: ReverseQueue m => m ()
summonNaurlhug = do
  dragons <- select $ EnemyWithCardCode (toCardCode Enemies.naurlhug)
  if null dragons
    then addSetAsideToStaging (cardIs Enemies.naurlhug)
    else for_ dragons \eid -> placeEnemy eid StagingArea
