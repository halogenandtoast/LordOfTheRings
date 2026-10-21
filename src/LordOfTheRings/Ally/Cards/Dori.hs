module LordOfTheRings.Ally.Cards.Dori (dori) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Dori = Dori AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dori :: AllyCard Dori
dori = ally Dori Cards.dori

-- | Response: After a hero is assigned any amount of damage, exhaust Dori to place that
-- damage on Dori instead.
instance HasAbilities Dori where
  getAbilities (Dori a) = [mkAbility a 1 $ ResponseAbility (HeroDealtDamageWindow After) (ExhaustCost $ toTarget a)]

instance RunMessage Dori where
  runMessage msg s@(Dori a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      chooseOrRunOneM pid do
        questionLabeled "dori.takeDamage"
        causedBy a
        for_ [(hero', n) | Window _ (DealtDamage hero'@(HeroTarget _) n) <- windows] \(hero', n) -> targeting hero' do
          -- the hero's damage is checked after this, so it is spared what Dori takes
          removeTokens a hero' Damage n
          placeTokens a a Damage n
          push $ CheckDefeated (toTarget a)
      pure s
    _ -> Dori <$> runMessage msg a
