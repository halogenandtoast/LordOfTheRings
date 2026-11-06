module LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight, currentNightObjective) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toId, toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities ()
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Id (ObjectiveId)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Prelude

data NightTime = Eventide | Dusk | Midnight
  deriving stock (Show, Eq)

-- Only the objective in play counts; earlier Night objectives are in victory.
currentNight :: HasGame m => m (Maybe NightTime)
currentNight = fmap fst <$> nightObjective

currentNightObjective :: HasGame m => m (Maybe ObjectiveId)
currentNightObjective = fmap snd <$> nightObjective

nightObjective :: HasGame m => m (Maybe (NightTime, ObjectiveId))
nightObjective = getsGame \g -> headMay
  [(time, toId o) | o <- toList g.entities.objectives, (toAttrs o).placement /= Unplaced, (time, def) <- [(Eventide, Cards.eventide), (Dusk, Cards.dusk), (Midnight, Cards.midnight)], toCardCode o == toCardCode def]
