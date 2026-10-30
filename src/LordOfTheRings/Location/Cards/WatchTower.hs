module LordOfTheRings.Location.Cards.WatchTower (watchTower) where

import LordOfTheRings.Game.Base (AttackDestroyed (attackDestroyedCard))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Raider))

newtype WatchTower = WatchTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

watchTower :: LocationCard WatchTower
watchTower = location WatchTower Cards.watchTower

{- | While Watch Tower is in the staging area, it gains: "Forced: When a Raider enemy enters
play, add 1 resource to it." Each enemy that enters play is given its time counters, so
that is where one is seen entering.
Shadow: If this attack destroys an ally, add resources to a Raider enemy in play equal to
that character's printed resource cost.
-}
instance RunMessage WatchTower where
  runMessage msg l@(WatchTower a) = case msg of
    PlaceTimeCounters (EnemyTarget eid) | a.placement == StagingArea -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) $ placeResources a eid 1
      WatchTower <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      cost <- sum . map (printedCostOf . attackDestroyedCard) <$> attackDestroyedAllies eid
      raiders <- select (EnemyWithTrait Raider)
      when (cost > 0) $ chooseOrRunOneM pid do
        questionLabeled "watchTower.raider"
        causedBy a
        targets raiders \r -> placeResources a r cost
      pure l
    _ -> WatchTower <$> runMessage msg a
