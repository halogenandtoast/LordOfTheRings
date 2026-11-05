module LordOfTheRings.Location.Cards.TheFrontGateNightmare (theFrontGateNightmare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Dragon))

newtype TheFrontGateNightmare = TheFrontGateNightmare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFrontGateNightmare :: LocationCard TheFrontGateNightmare
theFrontGateNightmare = location TheFrontGateNightmare Cards.theFrontGateNightmare

instance HasAbilities TheFrontGateNightmare where
  getAbilities (TheFrontGateNightmare a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage TheFrontGateNightmare where
  runMessage msg l@(TheFrontGateNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      dragons <- select (EnemyWithTrait Dragon)
      for_ dragons $ immediateAttack lead
      pure l
    _ -> TheFrontGateNightmare <$> runMessage msg a
