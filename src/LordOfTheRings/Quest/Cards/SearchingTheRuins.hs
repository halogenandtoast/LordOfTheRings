module LordOfTheRings.Quest.Cards.SearchingTheRuins (searchingTheRuins) where

import LordOfTheRings.Card (CardType (LocationType), cdCardType, toCardDef)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootStage)

newtype SearchingTheRuins = SearchingTheRuins QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingTheRuins :: QuestCard SearchingTheRuins
searchingTheRuins = quest SearchingTheRuins Cards.searchingTheRuins

-- | This stage cannot be defeated unless the players control at least 2 Loot objectives.
instance HasModifiersFor SearchingTheRuins where
  getModifiersFor (SearchingTheRuins a) = when a.flipped $ lootStage a 2

-- | Forced: At the beginning of the quest phase, the first player must choose: either raise
-- each player's threat by 2, or discard cards from the top of the encounter deck until a
-- location is discarded and add that location to the staging area.
instance HasAbilities SearchingTheRuins where
  getAbilities (SearchingTheRuins a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck. Then, each
-- player reveals an encounter card.
instance RunMessage SearchingTheRuins where
  runMessage msg q@(SearchingTheRuins attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      SearchingTheRuins <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      eachPlayer \_ -> revealEncounterCard
      pure q
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      chooseOneM pid do
        questionLabeled "searchingTheRuins.choose"
        causedBy attrs
        labeled "searchingTheRuins.raiseThreat" $ eachPlayer \p -> encounterRaisesThreat p 2
        labeled "searchingTheRuins.location" $ discardEncounterCardsUntil attrs (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ take 1 $ filter ((== LocationType) . cdCardType . toCardDef) cards
      pure q
    _ -> SearchingTheRuins <$> runMessage msg attrs
