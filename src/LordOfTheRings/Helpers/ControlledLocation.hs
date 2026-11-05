{- | Locations the players control, as Assault on Osgiliath (MEC rules sheet) and Wrath and
Ruin (MEC78) have them. A controlled location sits in its controller's play area
('InPlayArea'), still in play: its text is active and card effects affect it, and what is
attached to it stays. Taking control of a location removes its progress, and so does
returning it to the staging area, which loses its controller control of it. An eliminated
player's locations return to the staging area (the game does this as he leaves).
-}
module LordOfTheRings.Helpers.ControlledLocation (
  controlledLocations,
  takeControlOfLocation,
  firstPlayerTakesControl,
  returnToStaging,
  returnHighestThreatLocation,
  returnALocation,
  losesControlOfAnother,
) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Frontend (Frontend)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Id
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Classes.HasQueue (HasQueue)
import LordOfTheRings.Source (Source (GameSource))
import LordOfTheRings.Target (Target (LocationTarget), Targetable)
import LordOfTheRings.Token (Token (Progress))

-- | The locations the player controls.
controlledLocations :: HasGame m => PlayerId -> m [LocationId]
controlledLocations pid = select $ LocationControlledBy (PlayerWithId pid)

-- | The player takes control of the location: its progress is removed and it goes to
-- their play area.
takeControlOfLocation :: ReverseQueue m => PlayerId -> LocationId -> m ()
takeControlOfLocation pid lid = do
  removeProgress lid
  placeLocation lid (InPlayArea pid)

{- | "Forced: When a location is explored, the first player takes control of it." Read in
the When window of its exploring: he places it in his play area instead of its going to
the discard pile, or to the victory display.
-}
firstPlayerTakesControl
  :: (HasGame m, MonadRandom m, Frontend m, HasQueue Message m) => LocationId -> QueueT Message m ()
firstPlayerTakesControl lid = do
  cancelMatching \case
    Discard _ (LocationTarget l) -> l == lid
    AddToVictoryDisplay (LocationTarget l) -> l == lid
    _ -> False
  lead <- getLead
  takeControlOfLocation lead lid

-- | The location returns to the staging area, its controller losing it and its progress.
returnToStaging :: ReverseQueue m => LocationId -> m ()
returnToStaging lid = do
  removeProgress lid
  placeLocation lid StagingArea

removeProgress :: ReverseQueue m => LocationId -> m ()
removeProgress lid = do
  progress <- (.progress) <$> getLocation lid
  when (progress > 0) $ removeTokens GameSource lid Progress progress

-- | The player returns the location he controls with the highest threat to the staging
-- area, choosing among those tied.
returnHighestThreatLocation :: (ReverseQueue m, Targetable source) => source -> PlayerId -> m ()
returnHighestThreatLocation source pid = do
  threats <- traverse (\l -> (l,) <$> getLocationThreat l) =<< controlledLocations pid
  let highest = foldr (max . snd) 0 threats
  chooseOrRunOneM pid do
    questionLabeled "controlledLocation.returnHighest"
    causedBy source
    targets [l | (l, t) <- threats, t == highest] returnToStaging

-- | The player returns a location he controls to the staging area, if able.
returnALocation :: (ReverseQueue m, Targetable source) => source -> PlayerId -> m ()
returnALocation source pid = do
  locations <- controlledLocations pid
  chooseOrRunOneM pid do
    questionLabeled "controlledLocation.returnLocation"
    causedBy source
    targets locations returnToStaging

{- | Whether the message has the player who controls this location lose control of another
one, as "After the player who controls X loses control of a location" reads: it moves out
of his play area, or is discarded from it. No window opens for losing control, so the
location reads the message itself.
-}
losesControlOfAnother :: HasGame m => LocationAttrs -> Message -> m Bool
losesControlOfAnother a = \case
  PlaceLocation lid placement | lid /= a.id, controlled -> (\l -> l.placement == a.placement && placement /= a.placement) <$> getLocation lid
  Discard _ (LocationTarget lid) | lid /= a.id, controlled -> (== a.placement) . (.placement) <$> getLocation lid
  _ -> pure False
 where
  controlled = case a.placement of
    InPlayArea _ -> True
    _ -> False
