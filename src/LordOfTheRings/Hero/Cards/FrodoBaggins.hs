module LordOfTheRings.Hero.Cards.FrodoBaggins (frodoBaggins) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype FrodoBaggins = FrodoBaggins HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBaggins :: HeroCard FrodoBaggins
frodoBaggins = hero FrodoBaggins Cards.frodoBaggins

-- | Response: After Frodo Baggins is damaged, cancel the damage and instead raise your
-- threat by the amount of damage he would have been dealt. (Limit once per phase.)
instance HasAbilities FrodoBaggins where
  getAbilities (FrodoBaggins a) = [limited (PlayerLimit PerPhase 1) $ mkAbility a 1 $ response $ DealtDamageWindow After (toTarget a)]

instance RunMessage FrodoBaggins where
  runMessage msg h@(FrodoBaggins attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      let n = sum [d | Window _ (DealtDamage _ d) <- windows]
      removeTokens attrs attrs Damage (min n attrs.damage)
      raiseThreat attrs.owner n
      pure h
    _ -> FrodoBaggins <$> runMessage msg attrs
