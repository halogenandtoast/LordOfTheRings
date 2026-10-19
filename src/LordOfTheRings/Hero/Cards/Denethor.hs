module LordOfTheRings.Hero.Cards.Denethor (denethor) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Denethor = Denethor HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

denethor :: HeroCard Denethor
denethor = hero Denethor Cards.denethor

-- | Action: Exhaust Denethor to look at the top card of the encounter deck. You may move
-- that card to the bottom of the deck.
instance HasAbilities Denethor where
  getAbilities (Denethor a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage Denethor where
  runMessage msg h@(Denethor attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      lookAtTopOfEncounterDeck pid attrs True
      pure h
    _ -> Denethor <$> runMessage msg attrs
