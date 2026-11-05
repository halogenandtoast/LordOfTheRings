-- | What the Ship objectives of The Dream-chaser's Fleet share: they are immune to player
-- card effects, and leave play for good, an objective never going to the encounter
-- discard pile; destroyed, they cost their controller or the players the game.
module LordOfTheRings.Ally.Fleet (shipModifiers, runShip, shipController) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Queue (QueueT)

shipModifiers :: HasModifiersM m => AllyAttrs -> m ()
shipModifiers a = modified a a [ImmuneToPlayerCardEffects]

shipController :: AllyAttrs -> Maybe PlayerId
shipController a = placementController a.placement

-- | The ship's own run: destroyed, the loss given resolves; discarded, it leaves the game.
runShip :: QueueT Message GameT () -> Message -> AllyAttrs -> QueueT Message GameT AllyAttrs
runShip destroyed msg a = case msg of
  Destroy _ (isTarget a -> True) -> do
    destroyed
    runMessage msg a
  Discard _ (isTarget a -> True) -> do
    removeFromEncounterDiscard $ MkEncounterCard a.allyCardId (toCardCode a)
    runMessage msg a
  _ -> runMessage msg a
