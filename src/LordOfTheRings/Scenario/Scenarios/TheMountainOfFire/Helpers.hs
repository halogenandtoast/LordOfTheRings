-- | What The Mountain of Fire's encounter cards share: its The One Ring objective, which The
-- Tower of Cirith Ungol and Mount Doom attach to the Ring-bearer as they are set up, and
-- the stage the players are at.
module LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (
  theOneRing,
  readyOneRing,
  atStage,
) where

import LordOfTheRings.Card (toCardCode, toCardDef)
import LordOfTheRings.Card.CardDef (cdStage)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id (ObjectiveId)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (Objective)
import LordOfTheRings.Prelude
import LordOfTheRings.Card.CardDef (cdCardCode)

-- | The One Ring, in play.
theOneRing :: HasGame m => m (Maybe Objective)
theOneRing = getsGame (find ((== cdCardCode Objectives.theOneRingTheMountainOfFire) . toCardCode) . toList . entitiesObjectives . gameEntities)

-- | The One Ring, in play and ready, as "exhaust The One Ring" needs it.
readyOneRing :: HasGame m => m (Maybe ObjectiveId)
readyOneRing = (>>= \o -> toId o <$ guard (not o.exhausted)) <$> theOneRing

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
