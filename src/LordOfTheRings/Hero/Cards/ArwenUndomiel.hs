module LordOfTheRings.Hero.Cards.ArwenUndomiel (arwenUndomiel) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor))

newtype ArwenUndomiel = ArwenUndomiel HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

arwenUndomiel :: HeroCard ArwenUndomiel
arwenUndomiel = hero ArwenUndomiel Cards.arwenUndomiel

-- | Action: Discard a card from your hand to add 1 resource to a Noldor's resource pool,
-- or to Aragorn's resource pool. (Limit once per round.)
instance HasAbilities ArwenUndomiel where
  getAbilities (ArwenUndomiel a) =
    [limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (DiscardFromHandCost a.owner 1)]

instance RunMessage ArwenUndomiel where
  runMessage msg h@(ArwenUndomiel attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroMatchAny [HeroWithTrait Noldor, HeroWithTitle "Aragorn"])
      chooseOrRunOneM pid do
        questionLabeled "arwenUndomiel.resource"
        causedBy attrs
        targets heroes \x -> placeResources attrs x 1
      pure h
    _ -> ArwenUndomiel <$> runMessage msg attrs
