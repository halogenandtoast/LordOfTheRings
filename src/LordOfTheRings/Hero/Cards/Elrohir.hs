module LordOfTheRings.Hero.Cards.Elrohir (elrohir) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Elrohir = Elrohir HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

elrohir :: HeroCard Elrohir
elrohir = hero Elrohir Cards.elrohir

-- | While Elladan is in play, Elrohir gets +2 defense.
instance HasModifiersFor Elrohir where
  getModifiersFor (Elrohir a) = do
    elladan <- selectAny (CharacterWithTitle "Elladan")
    when elladan $ modifySelf a [DefenseModifier 2]

-- | Response: After Elrohir is declared as a defender, pay 1 resource from his resource
-- pool to ready him.
instance HasAbilities Elrohir where
  getAbilities (Elrohir a) =
    [mkAbility a 1 $ ResponseAbility (DefenderDeclaredWindow After (Just $ toTarget a)) (ResourceCost (toTarget a) 1)]

instance RunMessage Elrohir where
  runMessage msg h@(Elrohir attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ -> Elrohir <$> runMessage msg attrs
