-- | What The Black Gate Opens' cards share: the resources on The Black Gate, which Jaws of
-- Steel, stage 2B and the campaign card's resolution count.
module LordOfTheRings.Scenario.Scenarios.TheBlackGateOpens.Helpers (
  blackGateResources,
) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationTokens))
import LordOfTheRings.Prelude
import LordOfTheRings.Token (Token (Resource), countTokens)

-- | The resources on The Black Gate, none while it is not in play.
blackGateResources :: HasGame m => m Int
blackGateResources =
  getsGame
    $ sum
    . map (countTokens Resource . locationTokens . toAttrs)
    . filter ((== toCardCode Locations.theBlackGate) . toCardCode)
    . toList
    . entitiesLocations
    . gameEntities
