-- | What Return to Mirkwood's cards share: Gollum, and the player guarding him.
module LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (
  getGollum,
  getGuard,
  tantrumTargets,
  passGollum,
) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (ObjectiveId, PlayerId)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Prelude

-- | Gollum, while he is in play, and the player guarding him.
gollum :: (HasGame m) => m (Maybe (ObjectiveId, Maybe PlayerId))
gollum =
  getsGame
    $ fmap (\o -> (toId o, guardOf o.placement))
    . find ((== toCardCode Objectives.gollumReturnToMirkwood) . toCardCode)
    . toList
    . (.entities.objectives)
 where
  guardOf = \case
    InPlayArea pid -> Just pid
    _ -> Nothing

getGollum :: (HasGame m) => m (Maybe ObjectiveId)
getGollum = fmap fst <$> gollum

getGuard :: (HasGame m) => m (Maybe PlayerId)
getGuard = (>>= snd) <$> gollum

-- | The players a Tantrum card's effect reaches: the player guarding Gollum, or every
-- player while Dry Watercourse is the active location.
tantrumTargets :: (HasGame m) => m [PlayerId]
tantrumTargets = do
  dry <- selectAny (IsActiveLocation <> LocationWithCardCode (toCardCode Locations.dryWatercourse))
  if dry then getRemainingPlayers else maybeToList <$> getGuard

-- | The player guarding Gollum chooses a new player to guard him, if able; when it is
-- optional they may keep him. Not while The Spider's Ring is the active location.
passGollum :: (ReverseQueue m) => Bool -> m ()
passGollum optional = do
  locked <- selectAny (IsActiveLocation <> LocationWithCardCode (toCardCode Locations.theSpidersRing))
  gollum >>= \case
    Just (oid, Just guard') | not locked -> do
      others <- filter (/= guard') <$> getRemainingPlayers
      unless (null others) $ chooseOneM guard' do
        questionLabeled "returnToMirkwood.newGuard"
        targets others \p -> placeObjective oid (InPlayArea p)
        when optional $ labeled "pass" $ pure ()
    _ -> pure ()
