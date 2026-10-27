module LordOfTheRings.Hero.Cards.Elladan (elladan) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Elladan = Elladan HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

elladan :: HeroCard Elladan
elladan = hero Elladan Cards.elladan

-- | While Elrohir is in play, Elladan gets +2 attack.
instance HasModifiersFor Elladan where
  getModifiersFor (Elladan a) = do
    elrohir <- selectAny (CharacterWithTitle "Elrohir")
    when elrohir $ modifySelf a [AttackModifier 2]

-- | Response: After Elladan is declared as an attacker, pay 1 resource from his resource
-- pool to ready him.
instance HasAbilities Elladan where
  getAbilities (Elladan a) =
    [mkAbility a 1 $ ResponseAbility (AttackerDeclaredWindow After (Just $ toTarget a)) (ResourceCost (toTarget a) 1)]

instance RunMessage Elladan where
  runMessage msg h@(Elladan attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ -> Elladan <$> runMessage msg attrs
