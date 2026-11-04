-- | What The Mountain of Fire's encounter cards share: the stage the players are at.
module LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (
  atStage,
) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Card.CardDef (cdStage)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Prelude

-- | The main quest is at that stage, on its B side, with at least that much progress, as
-- Topmost Chamber reads "at least 18 progress on stage 2B" and Mount Doom "at least 20
-- progress on stage 3B".
atStage :: HasGame m => Int -> Int -> m Bool
atStage n progress =
  getsGame
    $ any (\q -> let a = toAttrs q in cdStage (toCardDef a) == Just n && a.flipped && a.progress >= progress)
    . toList
    . entitiesQuests
    . gameEntities
