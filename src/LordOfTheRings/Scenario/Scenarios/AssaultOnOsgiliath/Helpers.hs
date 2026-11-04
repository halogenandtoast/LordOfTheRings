{- | What the cards of Assault on Osgiliath share: the players take control of the
Osgiliath locations they explore, which sit in their play areas, and lose them back to the
staging area (see "LordOfTheRings.Helpers.ControlledLocation").
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
import LordOfTheRings.Helpers.ControlledLocation
import LordOfTheRings.Id
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Targetable)
import LordOfTheRings.Timing (Timing (After))

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
