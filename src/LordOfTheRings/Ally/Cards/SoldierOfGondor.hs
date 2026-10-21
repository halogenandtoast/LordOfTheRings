module LordOfTheRings.Ally.Cards.SoldierOfGondor (soldierOfGondor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Gondor))

newtype SoldierOfGondor = SoldierOfGondor AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

soldierOfGondor :: AllyCard SoldierOfGondor
soldierOfGondor = ally SoldierOfGondor Cards.soldierOfGondor

-- | Response: After Soldier of Gondor enters play, search the top 5 cards of your deck for
-- a Gondor ally and add it to your hand (any number of Gondor allies instead if your
-- threat is 40 or higher). Shuffle your deck.
instance HasAbilities SoldierOfGondor where
  getAbilities (SoldierOfGondor a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage SoldierOfGondor where
  runMessage msg e@(SoldierOfGondor a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      threat <- (.threat) <$> getPlayer pid
      searchDeck pid a 5 (CardWithTrait Gondor <> CardWithType AllyType) (threat >= 40)
      pure e
    _ -> SoldierOfGondor <$> runMessage msg a
