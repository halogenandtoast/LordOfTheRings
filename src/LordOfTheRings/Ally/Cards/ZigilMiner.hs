module LordOfTheRings.Ally.Cards.ZigilMiner (zigilMiner) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)

newtype ZigilMiner = ZigilMiner AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

zigilMiner :: AllyCard ZigilMiner
zigilMiner = ally ZigilMiner Cards.zigilMiner

-- | Action: Exhaust Zigil Miner and name a number to discard the top 2 cards of your
-- deck. If at least one of those cards has cost equal to the named number, choose a hero
-- you control. For each card that matches the named number, add 1 resource to that
-- hero's resource pool.
instance HasAbilities ZigilMiner where
  getAbilities (ZigilMiner a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage ZigilMiner where
  runMessage msg s@(ZigilMiner a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- take 2 . (.deck) <$> getPlayer pid
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      let costs = [x | c <- top, Just (StaticCost x) <- [cdCost (toCardDef c)]]
      chooseOneM pid do
        questionLabeled "zigilMiner.nameNumber"
        causedBy a
        for_ [0 .. 5] \n -> labeledWith "zigilMiner.number" [intVar "number" n] do
          discardTopOfDeck pid 2
          let matching = length (filter (== n) costs)
          when (matching > 0) $ chooseOneM pid do
            questionLabeled "zigilMiner.chooseHero"
            causedBy a
            targets heroes \h -> placeResources a h matching
      pure s
    _ -> ZigilMiner <$> runMessage msg a
