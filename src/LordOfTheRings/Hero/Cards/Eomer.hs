module LordOfTheRings.Hero.Cards.Eomer (eomer) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Eomer = Eomer HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eomer :: HeroCard Eomer
eomer = hero Eomer Cards.eomer

-- | Response: After a character leaves play, Éomer gets +2 attack until the end of the
-- round. (Limit once per round.)
instance HasAbilities Eomer where
  getAbilities (Eomer a) = [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ response $ CharacterLeftPlayWindow After AnyCard]

instance RunMessage Eomer where
  runMessage msg h@(Eomer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifier attrs attrs (AttackModifier 2)
      pure h
    _ -> Eomer <$> runMessage msg attrs
