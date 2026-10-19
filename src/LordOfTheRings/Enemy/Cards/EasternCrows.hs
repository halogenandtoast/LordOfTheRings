module LordOfTheRings.Enemy.Cards.EasternCrows (easternCrows) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EasternCrows = EasternCrows EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

easternCrows :: EnemyCard EasternCrows
easternCrows = enemy EasternCrows Cards.easternCrows

-- | Surge. Forced: After Eastern Crows is defeated, shuffle it back into the encounter deck.
instance HasModifiersFor EasternCrows where
  getModifiersFor (EasternCrows a) = modified a a [ShuffledIntoEncounterDeckWhenDefeated]

-- | Shadow: attacking enemy gets +1 attack. (+2 attack instead if defending player's
-- threat is 35 or higher.)
instance RunMessage EasternCrows where
  runMessage msg e@(EasternCrows attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      threat <- (.threat) <$> getPlayer pid
      enemyGetsAttack attrs eid (if threat >= 35 then 2 else 1)
      pure e
    _ -> EasternCrows <$> runMessage msg attrs
