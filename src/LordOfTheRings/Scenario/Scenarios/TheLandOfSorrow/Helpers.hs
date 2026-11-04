-- | What The Land of Sorrow's cards share: the Captive objective allies the Camp locations
-- guard, and the Warden enemy faceup under each Camp.
module LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (
  captivesControlled,
  campsInStaging,
  wardensUnder,
  wardensReleased,
  captiveLeavesPlay,
  runCaptive,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Ally.Runner
import LordOfTheRings.Card (Card (EncounterCard), EncounterCard)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Location.Types (LocationAttrs (locationUnderneath))
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Trait (Trait (Camp, Captive))

-- | The Captive objective allies the players control, as Watch Tower's X and stage 1B read.
captivesControlled :: HasGame m => m Int
captivesControlled = length <$> select (AllyWithTrait Captive <> AllyControlledBy AnyPlayer)

-- | The Camp locations in the staging area.
campsInStaging :: HasGame m => m Int
campsInStaging = length <$> select (LocationWithTrait Camp <> LocationInStagingArea)

-- | The Warden enemy faceup under the Camp, not in play.
wardensUnder :: LocationAttrs -> [EncounterCard]
wardensUnder a = [c | EncounterCard c <- locationUnderneath a]

-- | The enemies that were under the Camp, now in play: read in the step after they enter it.
wardensReleased :: HasGame m => LocationAttrs -> m [EnemyId]
wardensReleased a = concatMapM (select . EnemyWithCardId . (.id)) (wardensUnder a)

-- | "If this Captive leaves play, the players lose the game.": its ability 2.
captiveLeavesPlay :: AllyAttrs -> Ability
captiveLeavesPlay a = mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)

{- | A Captive objective ally, guarded by its Camp: as the Camp is explored, it leaves play
and the first player gains control of the ally it guarded, ready, as a guarded objective
is claimed (the Camp's Forced effect), and the players lose should it leave play.
-}
runCaptive :: Message -> AllyAttrs -> QueueT Message GameT AllyAttrs
runCaptive msg a = case msg of
  UseCardAbility _ (isSource a -> True) 2 _ -> do
    gameOver Lost
    pure a
  RescueCharacter (isTarget a -> True) -> do
    lead <- getLead
    pure $ a & placementL .~ InPlayArea lead
  _ -> runMessage msg a
