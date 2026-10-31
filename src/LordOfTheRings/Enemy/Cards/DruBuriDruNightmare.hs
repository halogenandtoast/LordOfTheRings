module LordOfTheRings.Enemy.Cards.DruBuriDruNightmare (druBuriDruNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Wose))

newtype DruBuriDruNightmare = DruBuriDruNightmare EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

druBuriDruNightmare :: EnemyCard DruBuriDruNightmare
druBuriDruNightmare = enemy DruBuriDruNightmare Cards.druBuriDruNightmare

-- | Drû-buri-Drû cannot take more than 1 damage each round: each time is at most 1, and
-- once he has taken it he takes no more this round.
-- While Drû-buri-Drû is in play, other Wose enemies get +1 attack and +1 defense.
-- Unless Drû-buri-Drû is in the victory display, the players cannot win: The Passage Out
-- holds it, as for the standard Drû-buri-Drû.
instance HasModifiersFor DruBuriDruNightmare where
  getModifiersFor (DruBuriDruNightmare a) = do
    modified a a [DamageReducedTo 1]
    woses <- select (EnemyWithTrait Wose)
    for_ (filter (/= a.id) woses) \w -> modified a w [AttackModifier 1, DefenseModifier 1]

instance HasAbilities DruBuriDruNightmare where
  getAbilities (DruBuriDruNightmare a) = [mkAbility a 1 $ silent $ DealtDamageWindow After (toTarget a)]

instance RunMessage DruBuriDruNightmare where
  runMessage msg e@(DruBuriDruNightmare attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifier attrs attrs CannotTakeDamage
      pure e
    _ -> DruBuriDruNightmare <$> runMessage msg attrs
