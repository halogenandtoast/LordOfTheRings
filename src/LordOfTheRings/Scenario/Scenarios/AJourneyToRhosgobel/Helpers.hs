-- | What A Journey to Rhosgobel's nightmare cards share: Wilyador and the Athelas cards.
module LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel.Helpers (getWilyador, athelasInPlay) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (AllyId, ObjectiveId)
import LordOfTheRings.Matcher
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Prelude

-- | Wilyador, in play as the first player's ally.
getWilyador :: HasGame m => m (Maybe AllyId)
getWilyador = headMay <$> select (AllyWithCardCode $ toCardCode Objectives.wilyador)

-- | The Athelas cards in play.
athelasInPlay :: HasGame m => m [ObjectiveId]
athelasInPlay =
  getsGame
    $ map toId
    . filter (\o -> toCardCode o == toCardCode Objectives.athelas && o.placement /= Unplaced)
    . toList
    . (.entities.objectives)
