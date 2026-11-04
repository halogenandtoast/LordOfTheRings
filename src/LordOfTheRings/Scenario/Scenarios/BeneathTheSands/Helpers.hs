-- | What the cards of Beneath the Sands share: the Search objective, On Track or Off Track as
-- it shows, and the On Track mark printed on some of its encounter cards.
module LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (
  getSearchObjective,
  isOnTrack,
  isOffTrack,
  flipSearchTo,
  turnSearchTo,
  onTrackMark,
) where

import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | The Search objective in play, whichever side is faceup.
getSearchObjective :: HasGame m => m (Maybe ObjectiveId)
getSearchObjective =
  getsGame
    $ fmap toId
    . find ((`elem` map toCardCode [Objectives.offTrack, Objectives.onTrack]) . toCardCode)
    . toList
    . entitiesObjectives
    . gameEntities

searchShows :: HasGame m => CardDef -> m Bool
searchShows side = getsGame $ any ((== toCardCode side) . toCardCode) . toList . entitiesObjectives . gameEntities

-- | "While On Track is in play".
isOnTrack :: HasGame m => m Bool
isOnTrack = searchShows Objectives.onTrack

isOffTrack :: HasGame m => m Bool
isOffTrack = searchShows Objectives.offTrack

-- | "Flip the Search objective to On Track": nothing if it shows that side already, or while
-- it cannot flip (Lost Underground).
flipSearchTo :: ReverseQueue m => CardDef -> m ()
flipSearchTo side = unlessM (searchShows side) $ getSearchObjective >>= traverse_ flipOver

-- | As 'flipSearchTo', even while it cannot flip, as Lost Underground flips it itself.
turnSearchTo :: ReverseQueue m => CardDef -> m ()
turnSearchTo side = unlessM (searchShows side) $ getSearchObjective >>= traverse_ (push . Flip . toTarget)

-- | The cards with On Track printed in their lower right corner (read off the card images).
onTrackMark :: CardMatcher
onTrackMark =
  CardMatchAny
    $ map
      cardIs
      [Enemies.hiveGuardian, Enemies.spiderBroodling, Locations.pitchBlackTunnel, Locations.nestOfVermin, Locations.blockedPassage]
