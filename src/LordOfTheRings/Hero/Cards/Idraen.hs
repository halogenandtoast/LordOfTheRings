module LordOfTheRings.Hero.Cards.Idraen (idraen) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Idraen = Idraen HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

idraen :: HeroCard Idraen
idraen = hero Idraen Cards.idraen

-- | Response: After a location is explored, ready Idraen.
instance HasAbilities Idraen where
  getAbilities (Idraen a) =
    [restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero) $ response $ AnyExploredWindow After]

instance RunMessage Idraen where
  runMessage msg h@(Idraen attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ -> Idraen <$> runMessage msg attrs
