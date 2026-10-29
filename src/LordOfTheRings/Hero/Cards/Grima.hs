module LordOfTheRings.Hero.Cards.Grima (grima) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Grima = Grima HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

grima :: HeroCard Grima
grima = hero Grima Cards.grima

-- | Action: Lower the cost of the next card you play from your hand this round by 1. That
-- card gains Doomed 1. (Limit once per round.)
instance HasAbilities Grima where
  getAbilities (Grima a) = [limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ ActionAbility Free]

instance RunMessage Grima where
  runMessage msg h@(Grima attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      roundModifiers attrs (PlayerTarget pid) [LowerNextCostOf AnyCard 1, NextCardGainsDoomed 1]
      pure h
    _ -> Grima <$> runMessage msg attrs
