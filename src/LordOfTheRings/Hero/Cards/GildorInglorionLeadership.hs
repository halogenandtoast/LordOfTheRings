module LordOfTheRings.Hero.Cards.GildorInglorionLeadership (gildorInglorionLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype GildorInglorionLeadership = GildorInglorionLeadership HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gildorInglorionLeadership :: HeroCard GildorInglorionLeadership
gildorInglorionLeadership = hero GildorInglorionLeadership Cards.gildorInglorionLeadership

-- | Action: Pay 1 resource from Gildor Inglorion's resource pool to choose a player to draw
-- 1 card. (Limit once per round.)
instance HasAbilities GildorInglorionLeadership where
  getAbilities (GildorInglorionLeadership a) =
    [limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (ResourceCost (toTarget a) 1)]

instance RunMessage GildorInglorionLeadership where
  runMessage msg h@(GildorInglorionLeadership attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      choosePlayerM pid attrs \p -> drawCards attrs p 1
      pure h
    _ -> GildorInglorionLeadership <$> runMessage msg attrs
