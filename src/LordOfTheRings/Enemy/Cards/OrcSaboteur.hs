module LordOfTheRings.Enemy.Cards.OrcSaboteur (orcSaboteur) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (chooseBattleground)
import LordOfTheRings.Trait (Trait (Battleground))

newtype OrcSaboteur = OrcSaboteur EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcSaboteur :: EnemyCard OrcSaboteur
orcSaboteur = enemy OrcSaboteur Cards.orcSaboteur

-- | Damage dealt by Orc Saboteur's attacks must be assigned to a Battleground location in
-- play, if able. Undefended, its damage goes there.
instance HasModifiersFor OrcSaboteur where
  getModifiersFor (OrcSaboteur a) = do
    battlegrounds <- select (LocationWithTrait Battleground)
    modifySelf a [UndefendedDamageTo (toTarget l) | l <- battlegrounds]

-- | Defended, the defender's damage from the attack goes there.
-- Shadow: If this attack destroys a character, deal 2 damage to a Battleground location in
-- play.
instance RunMessage OrcSaboteur where
  runMessage msg e@(OrcSaboteur attrs) = case msg of
    CheckWindows ws | defenders@(_ : _) <- [d | Window _ (DeclaredAsDefender d eid) <- ws, eid == attrs.id] -> do
      battlegrounds <- select (LocationWithTrait Battleground)
      for_ defenders \d -> for_ battlegrounds \l -> attackModifier attrs d (DamageRedirectedTo $ toTarget l)
      OrcSaboteur <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      chooseBattleground attrs pid \l -> dealDamage attrs l 2
      pure e
    _ -> OrcSaboteur <$> runMessage msg attrs
