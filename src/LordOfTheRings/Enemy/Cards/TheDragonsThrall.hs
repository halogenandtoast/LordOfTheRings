module LordOfTheRings.Enemy.Cards.TheDragonsThrall (theDragonsThrall) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (damageHrogarsHill)

newtype TheDragonsThrall = TheDragonsThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDragonsThrall :: EnemyCard TheDragonsThrall
theDragonsThrall = enemy TheDragonsThrall Cards.theDragonsThrall

-- | Forced: After The Dragon's Thrall engages you, either deal 3 damage to Hrogar's Hill,
-- or it makes an immediate attack against you.
instance HasAbilities TheDragonsThrall where
  getAbilities (TheDragonsThrall a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: If this attack destroys a character, deal 3 damage to Hrogar's Hill.
instance RunMessage TheDragonsThrall where
  runMessage msg e@(TheDragonsThrall attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOneM pid do
          questionLabeled "theDragonsThrall.choose"
          causedBy attrs
          labeled "theDragonsThrall.damage" $ damageHrogarsHill attrs 3
          labeled "theDragonsThrall.attack" $ immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      damageHrogarsHill attrs 3
      pure e
    _ -> TheDragonsThrall <$> runMessage msg attrs
