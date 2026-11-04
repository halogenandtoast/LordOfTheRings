module LordOfTheRings.Ally.Cards.LongLakeFisherman (longLakeFisherman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype LongLakeFisherman = LongLakeFisherman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

longLakeFisherman :: AllyCard LongLakeFisherman
longLakeFisherman = ally LongLakeFisherman Cards.longLakeFisherman

-- | Response: After Long Lake Fisherman enters play, name a number. Then, search the top 5
-- cards of your deck for a card whose printed cost is equal to that number and add it to
-- your hand. Shuffle your deck.
instance HasAbilities LongLakeFisherman where
  getAbilities (LongLakeFisherman a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage LongLakeFisherman where
  runMessage msg s@(LongLakeFisherman a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      chooseOneM pid do
        questionLabeled "longLakeFisherman.nameNumber"
        causedBy a
        -- no player card is printed with a cost above 9
        for_ [0 .. 9 :: Int] \n -> labeledWith "longLakeFisherman.number" [intVar "number" n] do
          searchDeck pid a 5 (CardWithPrintedCost n) False
      pure s
    _ -> LongLakeFisherman <$> runMessage msg a
