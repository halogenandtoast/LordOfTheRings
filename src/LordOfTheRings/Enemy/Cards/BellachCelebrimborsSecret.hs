module LordOfTheRings.Enemy.Cards.BellachCelebrimborsSecret (bellachCelebrimborsSecret) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (cardsUnderOrcsSearch, isScour)
import LordOfTheRings.Trait (Trait (Orc))

newtype BellachCelebrimborsSecret = BellachCelebrimborsSecret EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

bellachCelebrimborsSecret :: EnemyCard BellachCelebrimborsSecret
bellachCelebrimborsSecret = enemy BellachCelebrimborsSecret Cards.bellachCelebrimborsSecret

-- | X is the number of cards underneath The Orcs' Search.
instance HasModifiersFor BellachCelebrimborsSecret where
  getModifiersFor (BellachCelebrimborsSecret a) = do
    x <- cardsUnderOrcsSearch
    when (x > 0) $ modified a a [ThreatModifier x]

-- | Scour: Each player must search the encounter deck and discard pile for an Orc enemy and
-- add it to the staging area.
instance RunMessage BellachCelebrimborsSecret where
  runMessage msg e@(BellachCelebrimborsSecret attrs) = case msg of
    _ | isScour attrs msg -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscard pid attrs (CardWithTrait Orc <> CardWithType EnemyType)
      pure e
    _ -> BellachCelebrimborsSecret <$> runMessage msg attrs
