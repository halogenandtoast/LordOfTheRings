module LordOfTheRings.Enemy.Cards.OrcArbalesters (orcArbalesters) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))

newtype OrcArbalesters = OrcArbalesters EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcArbalesters :: EnemyCard OrcArbalesters
orcArbalesters = enemy OrcArbalesters Cards.orcArbalesters

-- | Archery X. X is the number of different resource icons (leadership, lore, spirit, or
-- tactics) on heroes in play, printed or gained.
instance HasModifiersFor OrcArbalesters where
  getModifiersFor (OrcArbalesters a) = do
    icons <- concatMapM heroResourceIcons =<< traverse getHero =<< select AnyHero
    let x = length $ ordNub icons
    when (x > 0) $ modified a a [GainKeyword (Archery x)]

-- | Shadow: attacking enemy gets +2 attack.
instance RunMessage OrcArbalesters where
  runMessage msg e@(OrcArbalesters attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> OrcArbalesters <$> runMessage msg attrs
