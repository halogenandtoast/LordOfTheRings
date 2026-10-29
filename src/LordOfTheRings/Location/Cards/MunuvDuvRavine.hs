module LordOfTheRings.Location.Cards.MunuvDuvRavine (munuvDuvRavine) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunland))

newtype MunuvDuvRavine = MunuvDuvRavine LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

munuvDuvRavine :: LocationCard MunuvDuvRavine
munuvDuvRavine = location MunuvDuvRavine Cards.munuvDuvRavine

-- | Dunland enemies get +1 attack and +1 defense.
instance HasModifiersFor MunuvDuvRavine where
  getModifiersFor (MunuvDuvRavine a) = do
    enemies <- select (EnemyWithTrait Dunland)
    for_ enemies \e -> modified a e [AttackModifier 1, DefenseModifier 1]

instance RunMessage MunuvDuvRavine where
  runMessage msg (MunuvDuvRavine a) = MunuvDuvRavine <$> runMessage msg a
