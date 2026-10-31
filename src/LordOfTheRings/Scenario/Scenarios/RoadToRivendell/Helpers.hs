module LordOfTheRings.Scenario.Scenarios.RoadToRivendell.Helpers (getArwen) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Id (AllyId)
import LordOfTheRings.Matcher (AllyMatcher (AllyWithCardCode), select)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude

-- | Arwen Undómiel, the objective ally the players escort, while she is in play.
getArwen :: HasGame m => m [AllyId]
getArwen = select (AllyWithCardCode $ toCardCode Objectives.arwenUndomielRoadToRivendell)
