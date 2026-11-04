module LordOfTheRings.Location.Cards.UrukGuardroom (urukGuardroom) where

import LordOfTheRings.Card (CardType (AllyType, EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Orc))

newtype UrukGuardroom = UrukGuardroom LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukGuardroom :: LocationCard UrukGuardroom
urukGuardroom = location UrukGuardroom Cards.urukGuardroom

-- | While Uruk Guardroom is in the staging area, increase the cost to play each ally by 1.
instance HasModifiersFor UrukGuardroom where
  getModifiersFor (UrukGuardroom a) =
    when (a.placement == StagingArea) $ modified a GameTarget [IncreaseCost (CardWithType AllyType) 1]

-- | Travel: Shuffle the encounter discard pile into the encounter deck and discard cards
-- until an Orc enemy is discarded. Add that enemy to the staging area. The deck is read
-- once it is shuffled.
instance RunMessage UrukGuardroom where
  runMessage msg l@(UrukGuardroom a) = case msg of
    TravelTo lid | lid == a.id -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      UrukGuardroom <$> runMessage msg a
    DoStep 1 (TravelTo lid) | lid == a.id -> do
      discardEncounterCardsUntil a orcEnemy
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (take 1 $ filter (cardMatches orcEnemy) cards) addToStagingFromDiscard
      pure l
    _ -> UrukGuardroom <$> runMessage msg a
   where
    orcEnemy = CardWithType EnemyType <> CardWithTrait Orc
