module LordOfTheRings.Enemy.Cards.ColdfellGiant (coldfellGiant) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ColdfellGiant = ColdfellGiant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

coldfellGiant :: EnemyCard ColdfellGiant
coldfellGiant = enemy ColdfellGiant Cards.coldfellGiant

-- | Forced: After Coldfell Giant engages you, deal 1 damage to 3 different characters you
-- control.
instance HasAbilities ColdfellGiant where
  getAbilities (ColdfellGiant a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: If the defending character has at least 1 damage token on it, attacking enemy
-- makes an additional attack after this one.
instance RunMessage ColdfellGiant where
  runMessage msg e@(ColdfellGiant attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        damageDifferent pid 3 =<< charactersControlledBy pid
      pure e
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      damaged <- anyM (fmap (> 0) . getDamage) defenders
      when damaged $ additionalAttack pid eid
      pure e
    _ -> ColdfellGiant <$> runMessage msg attrs
   where
    -- the player picks the characters one at a time, each dealt its damage
    damageDifferent :: ReverseQueue m => PlayerId -> Int -> [Target] -> m ()
    damageDifferent pid n cs
      | n <= 0 || null cs = pure ()
      | length cs <= n = for_ cs \c -> dealDamage attrs c 1
      | otherwise = chooseOneM pid do
          questionLabeled "coldfellGiant.damage"
          causedBy attrs
          targets cs \c -> do
            dealDamage attrs c 1
            damageDifferent pid (n - 1) (filter (/= c) cs)
