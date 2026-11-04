module LordOfTheRings.Quest.Cards.LeaderOfTheCult (leaderOfTheCult) where

import LordOfTheRings.Card (CardType (EnemyType), cdQuestPoints, toCardDef)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DangerInDorwinion.Helpers (addRandomSetAside, cultist)
import LordOfTheRings.Trait (Trait (Cultist))

newtype LeaderOfTheCult = LeaderOfTheCult QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

leaderOfTheCult :: QuestCard LeaderOfTheCult
leaderOfTheCult = quest LeaderOfTheCult Cards.leaderOfTheCult

{- | The Cultist cannot take more than X damage each round, where X is the printed quest
points of the active location. With no active location there are no printed quest points
to read, and X is 0.
-}
instance HasModifiersFor LeaderOfTheCult where
  getModifiersFor (LeaderOfTheCult a) = when a.flipped do
    active <- selectOne IsActiveLocation
    x <- maybe (pure 0) (fmap (fromMaybe 0 . cdQuestPoints . toCardDef) . getLocation) active
    cultists <- select cultist
    for_ cultists \e -> modified a e [DamageLimitEachRound x]

{- | When Revealed: Add 1 random set-aside Cultist enemy to the staging area. Shuffle the
encounter discard pile into the encounter deck and discard cards until X enemies are
discarded, where X is one less than the number of players. Add each enemy discarded this
way to the staging area. With one player X is 0, and nothing is discarded.
When the Cultist enemy is defeated, the players win the game.
-}
instance RunMessage LeaderOfTheCult where
  runMessage msg q@(LeaderOfTheCult attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      LeaderOfTheCult <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addRandomSetAside Cultist
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      x <- subtract 1 . length <$> getRemainingPlayers
      when (x > 0) $ discardEncounterCardsUntilCount attrs x (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure q
    Destroy _ (EnemyTarget eid) | attrs.flipped -> do
      defeated <- selectAny (EnemyWithId eid <> cultist)
      when defeated winGame
      LeaderOfTheCult <$> runMessage msg attrs
    _ -> LeaderOfTheCult <$> runMessage msg attrs
