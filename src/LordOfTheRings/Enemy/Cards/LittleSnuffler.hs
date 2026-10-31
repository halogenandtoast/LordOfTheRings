module LordOfTheRings.Enemy.Cards.LittleSnuffler (littleSnuffler) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (isScour, scour)
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (printedCost)

newtype LittleSnuffler = LittleSnuffler EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

littleSnuffler :: EnemyCard LittleSnuffler
littleSnuffler = enemy LittleSnuffler Cards.littleSnuffler

-- | When Revealed: Trigger Little Snuffler's Scour effect.
-- Scour: The first player discards a random card from his hand and assigns X damage among
-- locations in the staging area. X is the discarded card's cost. He assigns it 1 at a
-- time.
instance RunMessage LittleSnuffler where
  runMessage msg e@(LittleSnuffler attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      scour attrs
      pure e
    _ | isScour attrs msg -> do
      lead <- getLead
      hand <- (.hand) <$> getPlayer lead
      discarded <- headMay <$> shuffleM hand
      for_ discarded \card -> do
        discardFromHand lead card
        assign lead (printedCost card)
      pure e
    _ -> LittleSnuffler <$> runMessage msg attrs
   where
    assign :: ReverseQueue m => PlayerId -> Int -> m ()
    assign lead n = when (n > 0) do
      staged <- select LocationInStagingArea
      chooseOneM lead do
        questionLabeledWith "littleSnuffler.assign" [intVar "n" n]
        causedBy attrs
        targets staged \l -> do
          placeDamage attrs l 1
          assign lead (n - 1)
