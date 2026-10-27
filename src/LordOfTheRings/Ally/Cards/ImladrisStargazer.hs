module LordOfTheRings.Ally.Cards.ImladrisStargazer (imladrisStargazer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype ImladrisStargazer = ImladrisStargazer AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

imladrisStargazer :: AllyCard ImladrisStargazer
imladrisStargazer = ally ImladrisStargazer Cards.imladrisStargazer

-- | Action: Exhaust Imladris Stargazer to choose a player. That player looks at the top
-- 5 cards of his deck and then returns them to the top of his deck in any order.
instance HasAbilities ImladrisStargazer where
  getAbilities (ImladrisStargazer a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage ImladrisStargazer where
  runMessage msg s@(ImladrisStargazer a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      choosePlayerM pid a \p -> do
        deck <- (.deck) <$> getPlayer p
        returnToTopOfDeckInAnyOrder p a (take 5 deck)
      pure s
    _ -> ImladrisStargazer <$> runMessage msg a
