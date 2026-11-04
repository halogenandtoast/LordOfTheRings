module LordOfTheRings.Enemy.Cards.BigUruk (bigUruk) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype BigUruk = BigUruk EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bigUruk :: EnemyCard BigUruk
bigUruk = enemy BigUruk Cards.bigUruk

-- | Forced: After Big Uruk engages you, either raise your threat by 4, or Big Uruk makes an
-- immediate attack against you.
instance HasAbilities BigUruk where
  getAbilities (BigUruk a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack for each ally you control.
instance RunMessage BigUruk where
  runMessage msg e@(BigUruk attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOneM pid do
          questionLabeled "bigUruk.choose"
          causedBy attrs
          labeled "bigUruk.raiseThreat" $ encounterRaisesThreat pid 4
          labeled "bigUruk.attack" $ immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      allies <- length <$> select (AllyControlledBy $ PlayerWithId pid)
      when (allies > 0) $ enemyGetsAttack attrs eid allies
      pure e
    _ -> BigUruk <$> runMessage msg attrs
