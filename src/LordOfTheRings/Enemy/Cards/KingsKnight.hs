module LordOfTheRings.Enemy.Cards.KingsKnight (kingsKnight) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getDefenseAgainst)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)

newtype KingsKnight = KingsKnight EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

kingsKnight :: EnemyCard KingsKnight
kingsKnight = enemy KingsKnight Cards.kingsKnight

-- | Whether the enemy's attack deals the defenders no damage, as it stands.
dealsNoDamage :: HasGame m => EnemyId -> [Target] -> m Bool
dealsNoDamage eid defenders = do
  attack <- getEnemyAttack eid
  defense <- sum <$> traverse (getDefenseAgainst eid) defenders
  pure $ notNull defenders && attack <= defense

-- | Forced: After King's Knight attacks, if no damage was dealt by its attack, return it to
-- the staging area. Its damage is read as it is dealt to a defender.
-- Shadow: If no damage is dealt by this attack, return attacking enemy to the staging area
-- after this attack.
-- ponytail: the shadow reads the attack and defense as it resolves, before anything later
-- in the attack changes them
instance RunMessage KingsKnight where
  runMessage msg e@(KingsKnight attrs) = case msg of
    AttackDamage eid defender@(isCharacter -> True) | eid == attrs.id -> do
      whenM (dealsNoDamage eid [defender]) $ returnToStagingAfterAttack eid
      KingsKnight <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      whenM (dealsNoDamage eid defenders) $ returnToStagingAfterAttack eid
      pure e
    _ -> KingsKnight <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
