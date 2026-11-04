module LordOfTheRings.Quest.Cards.SneakPastTheGuards (sneakPastTheGuards) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SneakPastTheGuards = SneakPastTheGuards QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sneakPastTheGuards :: QuestCard SneakPastTheGuards
sneakPastTheGuards = quest SneakPastTheGuards Cards.sneakPastTheGuards

-- | Each enemy in the staging area cannot take damage and gets +X engagement cost, where X
-- is the number of progress tokens here.
-- The players cannot defeat this stage unless there are at least 2 resources here.
instance HasModifiersFor SneakPastTheGuards where
  getModifiersFor (SneakPastTheGuards a) = when a.flipped do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e $ CannotTakeDamage : [EngagementCostModifier a.progress | a.progress > 0]
    when (countTokens Resource a.questTokens < 2) $ modifySelf a [CannotBeDefeated]

{- | When Revealed: Shuffle the encounter discard pile into the encounter deck. Each player
discards cards from the encounter deck until an enemy is discarded and adds that enemy to
the staging area: player by player, step N the Nth player's, as the deck is as the one
before left it.

Forced: At the end of the round, reveal an encounter card. Then, place 1 resource here if
no enemies were engaged this round.

-}
instance RunMessage SneakPastTheGuards where
  runMessage msg q@(SneakPastTheGuards attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      SneakPastTheGuards <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getPlayerOrder
      when (n <= length players) do
        discardEncounterCardsUntil attrs (CardWithType EnemyType)
        doStep (n + 1) inner
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (take 1 $ filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure q
    EndRound | attrs.flipped -> do
      players <- getRemainingPlayers
      engaged <- or <$> for players \p -> elem EngagedAnEnemyThisRound <$> getModifiers (PlayerTarget p)
      revealEncounterCard
      unless engaged $ placeResources attrs attrs 1
      SneakPastTheGuards <$> runMessage msg attrs
    _ -> SneakPastTheGuards <$> runMessage msg attrs
