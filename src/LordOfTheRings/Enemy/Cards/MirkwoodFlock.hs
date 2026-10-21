module LordOfTheRings.Enemy.Cards.MirkwoodFlock (mirkwoodFlock) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Trait (Trait (Eagle))

newtype MirkwoodFlock = MirkwoodFlock EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodFlock :: EnemyCard MirkwoodFlock
mirkwoodFlock = enemy MirkwoodFlock Cards.mirkwoodFlock

-- | Only Eagle characters or characters with ranged can attack or defend against Mirkwood
-- Flock.
instance HasModifiersFor MirkwoodFlock where
  getModifiersFor (MirkwoodFlock a) = modified a a [OnlyFoughtBy Eagle Ranged]

-- | Shadow: If this attack is undefended, the damage must be placed on Wilyador.
instance RunMessage MirkwoodFlock where
  runMessage msg e@(MirkwoodFlock attrs) = case msg of
    ResolveShadowEffect cid eid _ Nothing | cid == attrs.cardId -> do
      damageOnWilyador attrs eid
      pure e
    _ -> MirkwoodFlock <$> runMessage msg attrs

-- | Until the end of the phase, the enemy's undefended damage goes to Wilyador.
damageOnWilyador :: (ReverseQueue m) => EnemyAttrs -> EnemyId -> m ()
damageOnWilyador attrs eid = do
  wilyador <- select (AllyWithCardCode $ toCardCode Objectives.wilyador)
  for_ wilyador \w -> phaseModifier attrs eid (UndefendedDamageTo $ toTarget w)
