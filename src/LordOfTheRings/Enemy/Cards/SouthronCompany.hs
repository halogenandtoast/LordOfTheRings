module LordOfTheRings.Enemy.Cards.SouthronCompany (southronCompany) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Battle, Siege))

newtype SouthronCompany = SouthronCompany EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronCompany :: EnemyCard SouthronCompany
southronCompany = enemy SouthronCompany Cards.southronCompany

-- | Southron Company gets +2 threat and +2 attack while the current quest card has the
-- battle or siege keyword.
instance HasModifiersFor SouthronCompany where
  getModifiersFor (SouthronCompany a) = do
    war <- (||) <$> currentQuestHas Battle <*> currentQuestHas Siege
    when war $ modified a a [ThreatModifier 2, AttackModifier 2]

-- | Shadow: attacking enemy gets +1 attack. (+2 attack instead if the current quest card
-- has a keyword.)
instance RunMessage SouthronCompany where
  runMessage msg e@(SouthronCompany attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      -- ponytail: the stages of this cycle's keywords are battle and siege; archery comes with siege
      keyword <- (||) <$> currentQuestHas Battle <*> currentQuestHas Siege
      enemyGetsAttack attrs eid (if keyword then 2 else 1)
      pure e
    _ -> SouthronCompany <$> runMessage msg attrs
