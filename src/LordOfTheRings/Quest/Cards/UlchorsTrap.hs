module LordOfTheRings.Quest.Cards.UlchorsTrap (ulchorsTrap) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers

newtype UlchorsTrap = UlchorsTrap QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

ulchorsTrap :: QuestCard UlchorsTrap
ulchorsTrap = quest UlchorsTrap Cards.ulchorsTrap

{- | City Guard cannot have more than 20 damage, and it gets -5 engagement cost for each
resource here.
This stage cannot be defeated while City Guard has hit points remaining.
-}
instance HasModifiersFor UlchorsTrap where
  getModifiersFor (UlchorsTrap a) = do
    cityGuardStageModifiers a 20
    when a.flipped
      $ whenM (selectAny $ cityGuard <> NotEnemy EnemyWithNoHitPointsRemaining)
      $ modifySelf a [CannotBeDefeated]

-- | Forced: At the end of the round, place 1 resource here.
instance HasAbilities UlchorsTrap where
  getAbilities (UlchorsTrap a) = cityGuardStageAbilities a

{- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
cards from the top until X enemies are discarded. X is the number of players. Add each enemy
discarded this way to the staging area. The deck is read for the discards once it is
shuffled, a step later.
When this stage is defeated, the players win the game: it is the last of the quest deck.
-}
instance RunMessage UlchorsTrap where
  runMessage msg q@(UlchorsTrap attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      UlchorsTrap <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players enemyCard
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches enemyCard) cards) addToStagingFromDiscard
      pure q
    _ -> UlchorsTrap <$> runCityGuardStage msg attrs
   where
    enemyCard = CardWithType EnemyType
