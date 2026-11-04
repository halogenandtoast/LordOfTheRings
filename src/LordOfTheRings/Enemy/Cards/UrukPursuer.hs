module LordOfTheRings.Enemy.Cards.UrukPursuer (urukPursuer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (addToOrcsArea, atOrcsStage)

newtype UrukPursuer = UrukPursuer EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukPursuer :: EnemyCard UrukPursuer
urukPursuer = enemy UrukPursuer Cards.urukPursuer

-- | If the players are not at the same stage as the Orcs, Uruk Pursuer gains surge (as it
-- is revealed; it only matters then).
instance HasModifiersFor UrukPursuer where
  getModifiersFor (UrukPursuer a) = unlessM atOrcsStage $ modifySelf a [GainKeyword Surge]

-- | When Revealed: Add Uruk Pursuer to the Orcs' area.
-- Shadow: If the players are not at the same stage as the Orcs, put Uruk Pursuer into
-- play engaged with you.
instance RunMessage UrukPursuer where
  runMessage msg e@(UrukPursuer attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addToOrcsArea attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      unlessM atOrcsStage $ putShadowEnemyIntoPlay eid cid pid
      pure e
    _ -> UrukPursuer <$> runMessage msg attrs
