module LordOfTheRings.Hero.Cards.Erkenbrand (erkenbrand) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Erkenbrand = Erkenbrand HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def.
erkenbrand :: HeroCard Erkenbrand
erkenbrand = hero Erkenbrand Cards.erkenbrand

-- | While Erkenbrand is defending, he gains: "Response: Deal 1 damage to Erkenbrand to
-- cancel a shadow effect just triggered."
instance HasAbilities Erkenbrand where
  getAbilities (Erkenbrand a) =
    [restricted a 1 EncounterEffectsCancelable $ response $ ShadowEffectAgainstWindow When (toTarget a)]

instance RunMessage Erkenbrand where
  runMessage msg h@(Erkenbrand attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      dealDamage attrs attrs 1
      cancelShadowEffects windows
      pure h
    _ -> Erkenbrand <$> runMessage msg attrs
