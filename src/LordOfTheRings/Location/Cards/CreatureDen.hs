module LordOfTheRings.Location.Cards.CreatureDen (creatureDen) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype CreatureDen = CreatureDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

creatureDen :: LocationCard CreatureDen
creatureDen = location CreatureDen Cards.creatureDen

-- | Response: After the players travel to Creature Den, search the Caves deck for a Dragon
-- Sign and attach it to Creature Den as a guarded objective. Shuffle the Caves deck.
instance HasAbilities CreatureDen where
  getAbilities (CreatureDen a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

{- | Travel: Shuffle the encounter discard pile into the encounter deck and discard cards
from the top until an enemy is discarded. Add that enemy to the staging area. The shuffled
deck is read back in a later step.
-}
instance RunMessage CreatureDen where
  runMessage msg l@(CreatureDen a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      caves <- getCavesDeck
      for_ (find (cardMatches $ cardIs Objectives.dragonSign) caves) \sign -> do
        takeFromCavesDeck sign
        putObjectiveIntoPlayAttached sign a
      shuffleCavesDeck
      pure l
    TravelTo lid | lid == a.id -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      CreatureDen <$> runMessage msg a
    DoStep 1 (TravelTo lid) | lid == a.id -> do
      discardEncounterCardsUntil a enemyCard
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ find (cardMatches enemyCard) cards
      pure l
    _ -> CreatureDen <$> runMessage msg a
   where
    enemyCard = CardWithType EnemyType
