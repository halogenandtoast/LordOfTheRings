module LordOfTheRings.Objective.Cards.EasterlingPursuit (easterlingPursuit) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype EasterlingPursuit = EasterlingPursuit ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingPursuit :: ObjectiveCard EasterlingPursuit
easterlingPursuit = objective EasterlingPursuit Cards.easterlingPursuit

-- | Forced: At the end of the round, place 1 resource here.
instance HasAbilities EasterlingPursuit where
  getAbilities (EasterlingPursuit a) = [mkAbility a 1 $ forced RoundEndsWindow]

{- | Then if there are at least 3 resources here, remove 3 of them and shuffle the encounter
discard pile into the encounter deck. Discard cards from the top until X enemies are
discarded, where X is the number of players. Add each enemy discarded this way to the
staging area.
The resources are counted once the 1 is placed, and the deck read once it is shuffled.
-}
instance RunMessage EasterlingPursuit where
  runMessage msg o@(EasterlingPursuit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      doStep 1 msg
      pure o
    DoStep 1 msg'@(UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      when (countTokens Resource attrs.objectiveTokens >= 3) do
        removeTokens attrs attrs Resource 3
        shuffleEncounterDiscardIntoDeck AnyCard
        doStep 2 msg'
      pure o
    DoStep 2 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players enemyCard
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (filter (cardMatches enemyCard) cards)
      pure o
    _ -> EasterlingPursuit <$> runMessage msg attrs
   where
    enemyCard = CardWithType EnemyType
