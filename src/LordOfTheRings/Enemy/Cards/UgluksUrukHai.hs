module LordOfTheRings.Enemy.Cards.UgluksUrukHai (ugluksUrukHai) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype UgluksUrukHai = UgluksUrukHai EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Toughness 1. Archery 1, as its card def says.
ugluksUrukHai :: EnemyCard UgluksUrukHai
ugluksUrukHai = enemy UgluksUrukHai Cards.ugluksUrukHai

-- | When Revealed: Either increase the pursuit value by 3, or reveal an additional
-- encounter card. The player who revealed it chooses.
instance RunMessage UgluksUrukHai where
  runMessage msg e@(UgluksUrukHai attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      chooseOneM you do
        questionLabeled "ugluksUrukHai.choose"
        causedBy attrs
        labeled "ugluksUrukHai.pursuit" $ increasePursuit 3
        labeled "ugluksUrukHai.reveal" revealEncounterCard
      pure e
    _ -> UgluksUrukHai <$> runMessage msg attrs
