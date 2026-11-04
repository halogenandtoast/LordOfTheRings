module LordOfTheRings.Location.Cards.ParchedWadi (parchedWadi) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Creature))

newtype ParchedWadi = ParchedWadi LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

parchedWadi :: LocationCard ParchedWadi
parchedWadi = location ParchedWadi Cards.parchedWadi

creatureEnemy :: CardMatcher
creatureEnemy = CardWithType EnemyType <> CardWithTrait Creature

-- | While Parched Wadi is in the staging area, each Creature enemy gets +1 threat and +1
-- attack.
instance HasModifiersFor ParchedWadi where
  getModifiersFor (ParchedWadi a) = when (a.placement == StagingArea) do
    creatures <- select (EnemyWithTrait Creature)
    for_ creatures \e -> modified a e [ThreatModifier 1, AttackModifier 1]

-- | Forced: After Parched Wadi becomes the active location, shuffle the encounter discard
-- pile into the encounter deck and discard cards from the top until a Creature enemy is
-- discarded. Add that enemy to the staging area.
instance HasAbilities ParchedWadi where
  getAbilities (ParchedWadi a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The cards are discarded once the shuffled deck is there to discard from.
instance RunMessage ParchedWadi where
  runMessage msg l@(ParchedWadi a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      discardEncounterCardsUntil a creatureEnemy
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (find (cardMatches creatureEnemy) cards)
      pure l
    _ -> ParchedWadi <$> runMessage msg a
