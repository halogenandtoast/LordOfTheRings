module LordOfTheRings.Enemy.Cards.ChieftanUfthak (chieftanUfthak) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ChieftanUfthak = ChieftanUfthak EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

chieftanUfthak :: EnemyCard ChieftanUfthak
chieftanUfthak = enemy ChieftanUfthak Cards.chieftanUfthak

-- | Chieftain Ufthak gets +2 attack for each resource token on him.
instance HasModifiersFor ChieftanUfthak where
  getModifiersFor (ChieftanUfthak a) = do
    let resources = countTokens Resource a.enemyTokens
    when (resources > 0) $ modified a a [AttackModifier (2 * resources)]

-- | Forced: After Chieftain Ufthak attacks, place 1 resource token on him.
instance HasAbilities ChieftanUfthak where
  getAbilities (ChieftanUfthak a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage ChieftanUfthak where
  runMessage msg e@(ChieftanUfthak attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ PlaceTokens (toSource attrs) (toTarget attrs) Resource 1
      pure e
    _ -> ChieftanUfthak <$> runMessage msg attrs
