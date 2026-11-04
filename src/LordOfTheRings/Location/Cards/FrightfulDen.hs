module LordOfTheRings.Location.Cards.FrightfulDen (frightfulDen) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FrightfulDen = FrightfulDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frightfulDen :: LocationCard FrightfulDen
frightfulDen = location FrightfulDen Cards.frightfulDen

-- | Forced: After Frightful Den becomes the active location, shuffle the encounter discard
-- pile into the encounter deck and discard cards from the top until X enemies are
-- discarded, where X is the number of players. Add each enemy discarded this way to the
-- staging area.
instance HasAbilities FrightfulDen where
  getAbilities (FrightfulDen a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The cards are discarded once the shuffled deck is there to discard from.
instance RunMessage FrightfulDen where
  runMessage msg l@(FrightfulDen a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount a players enemyCard
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches enemyCard) cards) addToStagingFromDiscard
      pure l
    _ -> FrightfulDen <$> runMessage msg a
   where
    enemyCard = CardWithType EnemyType
