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
--
-- Answered as the damage is about to be placed, so damage he cancels is never placed:
-- what triggers on his being damaged, as Muck Adder's Forced effect, does not.
instance HasAbilities FrodoBaggins where
  getAbilities (FrodoBaggins a) =
    [limited (PlayerLimit PerPhase 1) $ mkAbility a 1 $ response $ DealtDamageWindow When (toTarget a)]

instance RunMessage FrodoBaggins where
  runMessage msg h@(FrodoBaggins attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      let n = sum [d | Window _ (DealtDamage _ d) <- windows]
      cancelDamage attrs
      raiseThreat attrs.owner n
      pure h
    _ -> FrodoBaggins <$> runMessage msg attrs
