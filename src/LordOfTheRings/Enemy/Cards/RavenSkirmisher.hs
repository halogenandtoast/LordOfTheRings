module LordOfTheRings.Enemy.Cards.RavenSkirmisher (ravenSkirmisher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (activeLocationTimeX, removeTimeCounterFromActiveLocation)

newtype RavenSkirmisher = RavenSkirmisher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenSkirmisher :: EnemyCard RavenSkirmisher
ravenSkirmisher = enemy RavenSkirmisher Cards.ravenSkirmisher

-- | When Revealed: Either remove 1 time counter from the active location, or reveal an
-- additional encounter card. The first player chooses; with no time counter on the active
-- location to remove, the card is revealed.
-- Shadow: Attacking enemy gets +X attack where X is the Time X value on the active
-- location.
instance RunMessage RavenSkirmisher where
  runMessage msg e@(RavenSkirmisher attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      counters <- sum <$> (traverse (getTimeCounters . toTarget) =<< select IsActiveLocation)
      chooseOrRunOneM lead do
        questionLabeled "ravenSkirmisher.choose"
        causedBy attrs
        when (counters > 0) $ labeled "ravenSkirmisher.removeTimeCounter" $ removeTimeCounterFromActiveLocation attrs
        labeled "ravenSkirmisher.reveal" revealEncounterCard
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- activeLocationTimeX
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure e
    _ -> RavenSkirmisher <$> runMessage msg attrs
