module LordOfTheRings.Location.Cards.NestOfVermin (nestOfVermin) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Creature))

newtype NestOfVermin = NestOfVermin LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Track 3.
nestOfVermin :: LocationCard NestOfVermin
nestOfVermin = location NestOfVermin Cards.nestOfVermin

creatureEnemy :: CardMatcher
creatureEnemy = CardWithType EnemyType <> CardWithTrait Creature

-- | Travel: Shuffle the encounter discard pile into the encounter deck and discard cards
-- from the top until a Creature enemy is discarded. Add the discarded enemy to the staging
-- area. The shuffled deck is read back in a later step.
instance RunMessage NestOfVermin where
  runMessage msg l@(NestOfVermin attrs) = case msg of
    TravelTo lid | lid == attrs.id -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 2 msg
      pure l
    DoStep 2 (TravelTo lid) | lid == attrs.id -> do
      discardEncounterCardsUntil attrs creatureEnemy
      pure l
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ find (cardMatches creatureEnemy) cards
      pure l
    _ -> NestOfVermin <$> runMessage msg attrs
