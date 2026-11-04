module LordOfTheRings.Quest.Cards.TheCrossingOfAraw (theCrossingOfAraw) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheCrossingOfAraw = TheCrossingOfAraw QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCrossingOfAraw :: QuestCard TheCrossingOfAraw
theCrossingOfAraw = quest TheCrossingOfAraw Cards.theCrossingOfAraw

-- | Each enemy in the staging area gets -10 engagement cost.
-- While at least one player is engaged with an enemy, no more than 5 progress can be placed
-- here each round.
instance HasModifiersFor TheCrossingOfAraw where
  getModifiersFor (TheCrossingOfAraw a) = when a.flipped do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e [EngagementCostModifier (-10)]
    engaged <- selectAny (EnemyEngagedWith AnyPlayer)
    when engaged $ modifySelf a [ProgressPerRoundLimit 5]

{- | When Revealed: Add Warriors of the East to the staging area. Shuffle the encounter
discard pile into the encounter deck and discard cards from the top until X enemies are
discarded, where X is the number of players. Add each enemy discarded this way to the
staging area. The deck is read once it is shuffled.
When this stage is defeated, the players win the game: it is the last stage.
-}
instance RunMessage TheCrossingOfAraw where
  runMessage msg q@(TheCrossingOfAraw attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheCrossingOfAraw <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Cards.warriorsOfTheEast)
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players enemyCard
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (filter (cardMatches enemyCard) cards)
      pure q
    _ -> TheCrossingOfAraw <$> runMessage msg attrs
   where
    enemyCard = CardWithType EnemyType
