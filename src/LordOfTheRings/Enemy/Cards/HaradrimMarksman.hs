module LordOfTheRings.Enemy.Cards.HaradrimMarksman (haradrimMarksman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery, Battle, Siege))

newtype HaradrimMarksman = HaradrimMarksman EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimMarksman :: EnemyCard HaradrimMarksman
haradrimMarksman = enemy HaradrimMarksman Cards.haradrimMarksman

-- | While the current quest has battle, Haradrim Marksman gets +2 threat and gains archery
-- 2.
-- While the current quest has siege, Haradrim Marksman gets +2 attack and allies cannot be
-- declared as defenders against its attacks.
instance HasModifiersFor HaradrimMarksman where
  getModifiersFor (HaradrimMarksman a) = do
    battle <- currentQuestHas Battle
    siege <- currentQuestHas Siege
    when battle $ modified a a [ThreatModifier 2, GainKeyword (Archery 2)]
    when siege $ modified a a [AttackModifier 2, CannotBeDefendedByAllies]

instance RunMessage HaradrimMarksman where
  runMessage msg (HaradrimMarksman attrs) = HaradrimMarksman <$> runMessage msg attrs
