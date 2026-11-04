module LordOfTheRings.Enemy.Cards.SentinelOfTheDeep (sentinelOfTheDeep) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (chooseLostEffects)

newtype SentinelOfTheDeep = SentinelOfTheDeep EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sentinelOfTheDeep :: EnemyCard SentinelOfTheDeep
sentinelOfTheDeep = enemy SentinelOfTheDeep Cards.sentinelOfTheDeep

-- | While Sentinel of the Deep is engaged with a player, that player cannot draw cards from
-- card effects.
instance HasModifiersFor SentinelOfTheDeep where
  getModifiersFor (SentinelOfTheDeep a) = for_ [pid | EngagedWith pid <- [a.placement]] \pid ->
    modified a (PlayerTarget pid) [CannotDrawCardsFromCardEffects]

-- | Shadow: Defending player must choose and trigger 1 'Lost:' effect in play.
instance RunMessage SentinelOfTheDeep where
  runMessage msg e@(SentinelOfTheDeep attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseLostEffects attrs [pid]
      pure e
    _ -> SentinelOfTheDeep <$> runMessage msg attrs
