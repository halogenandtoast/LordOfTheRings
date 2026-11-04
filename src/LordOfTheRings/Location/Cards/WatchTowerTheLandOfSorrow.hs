module LordOfTheRings.Location.Cards.WatchTowerTheLandOfSorrow (watchTowerTheLandOfSorrow) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (captivesControlled)

newtype WatchTowerTheLandOfSorrow = WatchTowerTheLandOfSorrow LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

watchTowerTheLandOfSorrow :: LocationCard WatchTowerTheLandOfSorrow
watchTowerTheLandOfSorrow = location WatchTowerTheLandOfSorrow Cards.watchTowerTheLandOfSorrow

-- | X threat. While Watch Tower is in the staging area, it gains archery X. X is 1 more
-- than the number of Captive objective allies the players control.
instance HasModifiersFor WatchTowerTheLandOfSorrow where
  getModifiersFor (WatchTowerTheLandOfSorrow a) = do
    x <- (+ 1) <$> captivesControlled
    modifySelf a $ ThreatModifier x : [GainKeyword (Archery x) | a.placement == StagingArea]

-- | Travel: Reveal the top card of the encounter deck.
instance RunMessage WatchTowerTheLandOfSorrow where
  runMessage msg (WatchTowerTheLandOfSorrow a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      WatchTowerTheLandOfSorrow <$> runMessage msg a
    _ -> WatchTowerTheLandOfSorrow <$> runMessage msg a
