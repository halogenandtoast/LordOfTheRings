module LordOfTheRings.Enemy.Cards.OrcRavagerNightmare (orcRavagerNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcRavagerNightmare = OrcRavagerNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcRavagerNightmare :: EnemyCard OrcRavagerNightmare
orcRavagerNightmare = enemy OrcRavagerNightmare Cards.orcRavagerNightmare

-- | Forced: When Orc Ravager is dealt a shadow card, it gets +2 defense until the end of
-- the phase.
-- Shadow: Exhaust a character you control. Deal attacking enemy another shadow card.
instance RunMessage OrcRavagerNightmare where
  runMessage msg e@(OrcRavagerNightmare attrs) = case msg of
    AttachShadowCard eid _ | eid == attrs.id -> do
      phaseModifier attrs attrs (DefenseModifier 2)
      OrcRavagerNightmare <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      dealShadowCardsDuringAttack eid pid defenders 1
      pure e
    _ -> OrcRavagerNightmare <$> runMessage msg attrs
