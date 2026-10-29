{- | What the cards of Assault on Osgiliath share: the players take control of the
Osgiliath locations they explore, which sit in their play areas, and lose them back to the
staging area. A location returned to the staging area loses its progress (rules sheet).
-}
module LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers (
  controlledLocations,
  takeControlOfLocation,
  returnToStaging,
  returnHighestThreatLocation,
  returnALocation,
  returnsWhenUndefended,
  destroyedReturnLocations,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Id
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Source (GameSource))
import LordOfTheRings.Target (Targetable)
import LordOfTheRings.Timing (Timing (After))
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
    questionLabeled "osgiliath.returnHighest"
    causedBy source
    targets [l | (l, t) <- threats, t == highest] returnToStaging

-- | The player returns a location he controls to the staging area, if able.
returnALocation :: (ReverseQueue m, Targetable source) => source -> PlayerId -> m ()
returnALocation source pid = do
  locations <- controlledLocations pid
  chooseOrRunOneM pid do
    questionLabeled "osgiliath.returnLocation"
    causedBy source
    targets locations returnToStaging

-- | "Forced: After the player who controls this location lets an attack go undefended,
-- return it to the staging area."
returnsWhenUndefended :: LocationAttrs -> Int -> [Ability]
returnsWhenUndefended a idx = [mkAbility a idx $ forced $ AttackUndefendedWindow After pid | InPlayArea pid <- [a.placement]]

-- | The shadow "If this attack destroys a character, that character's controller must
-- return a location he controls with the highest threat to the staging area", once the
-- enemy's attack is over: once for each character it destroyed.
destroyedReturnLocations :: (ReverseQueue m, Targetable source) => source -> EnemyId -> m ()
destroyedReturnLocations source eid =
  traverse_ (returnHighestThreatLocation source) . mapMaybe (.controller) =<< attackDestroyed eid
