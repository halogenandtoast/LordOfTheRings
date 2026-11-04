module LordOfTheRings.Quest.Cards.LeadTheRevolt (leadTheRevolt) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType), cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Warden))

newtype LeadTheRevolt = LeadTheRevolt QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

leadTheRevolt :: QuestCard LeadTheRevolt
leadTheRevolt = quest LeadTheRevolt Cards.leadTheRevolt

-- | This stage gets +10 quest points per player.
instance HasModifiersFor LeadTheRevolt where
  getModifiersFor (LeadTheRevolt a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [QuestPointModifier (10 * players)]

-- | Forced: After a player commits characters to the quest, if he is not engaged with a
-- Warden enemy he must reveal an encounter card.
instance HasAbilities LeadTheRevolt where
  getAbilities (LeadTheRevolt a) = [mkAbility a 1 $ forced $ AnyCommittedToQuestWindow After | a.flipped]

wardenEnemy :: EnemyMatcher
wardenEnemy = EnemyWithTrait Warden

{- | When Revealed: The first player returns each Warden enemy in the victory display to the
staging area. Each other player discards cards from the top of the encounter deck until he
discards an enemy and adds that enemy to the staging area: one player after another, each
discarding from the deck as the one before left it.

This stage cannot be defeated while a Warden enemy is in play. When this stage is defeated,
the players win the game.
-}
instance RunMessage LeadTheRevolt where
  runMessage msg q@(LeadTheRevolt attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      LeadTheRevolt <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      display <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      for_ [c | EncounterCard c <- display, Warden `member` cdCardTraits (toCardDef c)] \c -> do
        removeFromVictoryDisplay (EncounterCard c)
        addToStaging c
      doStep 1 msg
      pure q
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      others <- otherPlayers
      for_ (take 1 $ drop (n - 1) others) \p -> do
        discardEncounterCardsUntilFor attrs (PlayerTarget p) (CardWithType EnemyType)
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget _) cards -> do
      traverse_ addToStagingFromDiscard $ take 1 $ filter (cardMatches $ CardWithType EnemyType) cards
      pure q
    -- a player's characters are committed together, in one window
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      players <- nub . catMaybes <$> traverse characterController [c | Window _ (CommittedToQuest c) <- ws]
      for_ players \pid -> do
        engaged <- selectAny (wardenEnemy <> EnemyEngagedWith (PlayerWithId pid))
        unless engaged revealEncounterCard
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    _ -> LeadTheRevolt <$> runBlockedStage wardenEnemy msg attrs
   where
    otherPlayers = do
      lead <- getLead
      filter (/= lead) <$> getRemainingPlayers
