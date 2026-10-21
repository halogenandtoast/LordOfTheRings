module LordOfTheRings.Hero.Cards.Nori (nori) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype Nori = Nori HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

nori :: HeroCard Nori
nori = hero Nori Cards.nori

-- | Response: After you play a Dwarf character from your hand, reduce your threat by 1.
instance HasAbilities Nori where
  getAbilities (Nori a) =
    [mkAbility a 1 $ response $ PlayedCardFromHandWindow After a.heroOwner (CardWithTrait Dwarf <> CardWithType AllyType)]

instance RunMessage Nori where
  runMessage msg h@(Nori attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      lowerThreat pid 1
      pure h
    _ -> Nori <$> runMessage msg attrs
