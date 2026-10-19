module LordOfTheRings.Hero.Cards.Beravor (beravor) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Beravor = Beravor HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beravor :: HeroCard Beravor
beravor = hero Beravor Cards.beravor

-- | Action: Exhaust Beravor to choose a player. That player draws 2 cards. Limit once
-- per round.
instance HasAbilities Beravor where
  getAbilities (Beravor a) = [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage Beravor where
  runMessage msg h@(Beravor attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      push =<< choosePlayer pid (Just $ toTarget attrs) \p -> [DrawCards $ Drawing p 2]
      pure h
    _ -> Beravor <$> runMessage msg attrs
