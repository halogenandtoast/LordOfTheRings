-- | What The Mountain of Fire's encounter cards share: its The One Ring objective, which The
-- Tower of Cirith Ungol and Mount Doom attach to the Ring-bearer as they are set up.
module LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (
  theOneRing,
  readyOneRing,
) where

import LordOfTheRings.Card (toCardCode)
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
