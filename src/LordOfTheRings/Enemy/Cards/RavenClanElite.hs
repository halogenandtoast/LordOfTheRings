module LordOfTheRings.Enemy.Cards.RavenClanElite (ravenClanElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (removeTimeCountersFromLocations)

newtype RavenClanElite = RavenClanElite EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenClanElite :: EnemyCard RavenClanElite
ravenClanElite = enemy RavenClanElite Cards.ravenClanElite

-- | When Revealed: Either remove 1 time counter from a location in play, or Raven Clan
-- Elite makes an immediate attack against the first player. The first player chooses.
instance RunMessage RavenClanElite where
  runMessage msg e@(RavenClanElite attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      timed <- filterM (fmap (> 0) . getTimeCounters . toTarget) =<< select AnyLocation
      chooseOrRunOneM lead do
        questionLabeled "ravenClanElite.choose"
        causedBy attrs
        unless (null timed) $ labeled "ravenClanElite.removeTimeCounter" $ removeTimeCountersFromLocations attrs lead 1
        labeled "ravenClanElite.attack" $ immediateAttack lead attrs.id
      pure e
    _ -> RavenClanElite <$> runMessage msg attrs
