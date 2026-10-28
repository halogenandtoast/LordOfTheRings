module LordOfTheRings.Enemy.Cards.OrcScramblers (orcScramblers) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (besiegerShadow)
import LordOfTheRings.Trait (Trait (Battleground))

newtype OrcScramblers = OrcScramblers EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcScramblers :: EnemyCard OrcScramblers
orcScramblers = enemy OrcScramblers Cards.orcScramblers

-- | When Revealed: Deal 1 damage to each Battleground location in play, if able.
-- Shadow: Deal 2 damage to The Citadel if it is in play. Otherwise, attacking enemy gets
-- +2 attack.
instance RunMessage OrcScramblers where
  runMessage msg e@(OrcScramblers attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      battlegrounds <- select (LocationWithTrait Battleground)
      for_ battlegrounds \l -> dealDamage attrs l 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      besiegerShadow attrs Locations.theCitadel eid
      pure e
    _ -> OrcScramblers <$> runMessage msg attrs
