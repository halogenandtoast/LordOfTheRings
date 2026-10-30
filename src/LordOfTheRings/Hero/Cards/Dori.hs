module LordOfTheRings.Hero.Cards.Dori (dori) where

import LordOfTheRings.Helpers.Character (getDefense)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Dori = Dori HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def.
dori :: HeroCard Dori
dori = hero Dori Cards.dori

-- | Response: After another hero is declared as a defender, exhaust Dori to add his
-- defense to the defending hero's defense for this attack.
instance HasAbilities Dori where
  getAbilities (Dori a) =
    [mkAbility a 1 $ ResponseAbility (HeroDeclaredAsDefenderWindow After (not_ $ HeroWithId a.id)) (exhaustCost a)]

instance RunMessage Dori where
  runMessage msg h@(Dori attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      defense <- getDefense (toTarget attrs)
      for_ (take 1 [d | Window _ (DeclaredAsDefender d@(HeroTarget _) _) <- windows, d /= toTarget attrs]) \d ->
        attackModifier attrs d (DefenseModifier defense)
      pure h
    _ -> Dori <$> runMessage msg attrs
